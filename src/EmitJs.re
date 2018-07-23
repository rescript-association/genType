open GenFlowCommon;

module Convert = {
  let rec toString = converter =>
    switch (converter) {
    | CodeItem.Unit => "unit"
    | Identity => "id"
    | OptionalArgument(c) => "optionalArgument(" ++ toString(c) ++ ")"
    | Option(c) => "option(" ++ toString(c) ++ ")"
    | Fn((args, c)) =>
      let labelToString = label =>
        switch (label) {
        | NamedArgs.Nolabel => "-"
        | Label(l) => l
        | OptLabel(l) => "?" ++ l
        };
      "fn("
      ++ (
        args
        |> List.map(((label, conv)) =>
             "(~" ++ labelToString(label) ++ ":" ++ toString(conv) ++ ")"
           )
        |> String.concat(", ")
      )
      ++ " -> "
      ++ toString(c)
      ++ ")";
    };

  let rec apply = (~converter, ~toJS, s) =>
    switch (converter) {
    | CodeItem.Unit => s
    | Identity => s
    | OptionalArgument(c) => apply(~converter=c, ~toJS, s)
    | Option(c) =>
      let convertedArg = apply(~converter=c, ~toJS, s);
      toJS ?
        convertedArg :
        "("
        ++ convertedArg
        ++ " === null ? undefined : "
        ++ convertedArg
        ++ ")";
    | Fn((args, resultConverter))
        when
          args
          |> List.for_all(((_label, argConverter)) =>
               argConverter == CodeItem.Identity
             ) =>
      s |> apply(~converter=resultConverter, ~toJS)

    | Fn((args, resultConverter)) =>
      let convertArg = {
        let cnt = ref(0);
        (
          ((lbl, argConverter)) => {
            let varName =
              switch (lbl) {
              | NamedArgs.Nolabel =>
                incr(cnt);
                "Arg" ++ string_of_int(cnt^);
              | Label(l)
              | OptLabel(l) => "Arg" ++ l
              };
            let notToJS = !toJS;
            (
              varName,
              varName |> apply(~converter=argConverter, ~toJS=notToJS),
            );
          }
        );
      };
      let mkReturn = {
        let result = "result";
        (
          e =>
            "const "
            ++ result
            ++ " = "
            ++ e
            ++ "; return "
            ++ apply(~converter=resultConverter, ~toJS, result)
        );
      };
      let convertedArgs = args |> List.map(convertArg);
      let funArgs =
        "(" ++ (convertedArgs |> List.map(fst) |> String.concat(", ")) ++ ")";
      let funBody =
        s
        ++ "("
        ++ (convertedArgs |> List.map(snd) |> String.concat(", "))
        ++ ")";
      "(function " ++ funArgs ++ " { " ++ mkReturn(funBody) ++ " })";
    };
};

type env = {
  requires: StringMap.t(string),
  typeMap: StringMap.t(Flow.typ),
  exports: list((string, option(Flow.typ))),
};

let requireModule = (~env, moduleName) => {
  /* TODO: find the path from the module name */
  let path = Filename.(concat(parent_dir_name, moduleName ++ ".bs"));
  let requires = env.requires |> StringMap.add(moduleName, path);
  (requires, moduleName);
};

let emitCodeItems = codeItems => {
  let requireBuffer = Buffer.create(100);
  let mainBuffer = Buffer.create(100);
  let exportBuffer = Buffer.create(100);
  let line_ = (buffer, s) => {
    Buffer.add_string(buffer, s);
    Buffer.add_string(buffer, "\n");
  };
  let line = line_(mainBuffer);
  let emitExport = ((id, flowTypeOpt)) => {
    let addType = s =>
      switch (flowTypeOpt) {
      | None => s
      | Some(flowType) => "(" ++ s ++ ": " ++ Flow.render(flowType) ++ ")"
      };
    line_(exportBuffer, "exports." ++ id ++ " = " ++ addType(id) ++ ";");
  };
  let emitRequire = (id, v) =>
    line_(requireBuffer, "var " ++ id ++ " = require(\"" ++ v ++ "\");");
  let codeItem = (env, codeItem) =>
    switch (codeItem) {
    | CodeItem.RawJS(s) =>
      line(s ++ ";");
      env;
    | FlowTypeBinding(id, flowType) => {
        ...env,
        typeMap: env.typeMap |> StringMap.add(id, flowType),
      }
    | FlowAnnotation(annotationBindingName, constructorFlowType) =>
      line("// TODO: FlowAnnotation");
      env;
    | ValueBinding(inputModuleName, id, converter) =>
      let name = Ident.name(id);
      let (requires, moduleStr) = inputModuleName |> requireModule(~env);
      line(
        "const "
        ++ name
        ++ " = "
        ++ (
          moduleStr ++ "." ++ name |> Convert.apply(~converter, ~toJS=true)
        )
        ++ ";",
      );
      let flowType = env.typeMap |> StringMap.find(name);
      {
        ...env,
        exports: [(Ident.name(id), Some(flowType)), ...env.exports],
        requires,
      };
    | ConstructorBinding(
        constructorAlias,
        convertableFlowTypes,
        modulePath,
        leafName,
      ) =>
      line("// TODO: ConstructorBinding");
      env;
    | ComponentBinding(
        inputModuleName,
        flowPropGenerics,
        id,
        converter,
        propsTypeName,
      ) =>
      let name = "component";
      let jsProps = "jsProps";
      let jsPropsDot = s => jsProps ++ "." ++ s;
      let componentType =
        switch (flowPropGenerics) {
        | None => None
        | Some(flowType) =>
          Some(
            Flow.Ident(
              "React$ComponentType",
              [Flow.Ident(propsTypeName, [])],
            ),
          )
        };
      let args =
        switch (converter) {
        | CodeItem.Fn((argsConverter, _retConverter)) when argsConverter != [] =>
          switch (List.rev(argsConverter)) {
          | [] => assert(false)
          | [(_, childrenConverter), ...revPropConverters] =>
            [
              jsPropsDot("children")
              |> Convert.apply(~converter=childrenConverter, ~toJS=true),
              ...revPropConverters
                 |> List.map(((lbl, argConverter)) =>
                      switch (lbl) {
                      | NamedArgs.Label(l)
                      | OptLabel(l) =>
                        jsPropsDot(l)
                        |> Convert.apply(~converter=argConverter, ~toJS=true)
                      | Nolabel => assert(false)
                      }
                    ),
            ]
            |> List.rev
          }

        | _ => [jsPropsDot("children")]
        };

      let mkArgs = args => "(" ++ (args |> String.concat(", ")) ++ ")";

      line("const " ++ name ++ " = ReasonReact.wrapReasonForJs(");
      line("  " ++ inputModuleName ++ ".component" ++ ",");
      line(
        "  (function (" ++ jsProps ++ ": {...Props, children:any}" ++ ") {",
      );
      line(
        "     return "
        ++ inputModuleName
        ++ "."
        ++ Ident.name(id)
        ++ mkArgs(args)
        ++ ";",
      );
      line("  }));");
      {
        ...env,
        exports: [(name, componentType), ...env.exports],
        requires:
          env.requires
          |> StringMap.add("ReasonReact", "reason-react/src/ReasonReact.js"),
      };
    };
  let {requires, exports} =
    List.fold_left(
      codeItem,
      {typeMap: StringMap.empty, exports: [], requires: StringMap.empty},
      codeItems,
    );
  requires |> StringMap.iter(emitRequire);
  exports |> List.rev |> List.iter(emitExport);
  "\n"
  ++ (requireBuffer |> Buffer.to_bytes)
  ++ "\n"
  ++ (mainBuffer |> Buffer.to_bytes)
  ++ "\n"
  ++ (exportBuffer |> Buffer.to_bytes);
};