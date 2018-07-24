open GenFlowCommon;

module Emit = {
  let arg = x => "Arg" ++ x;
  let argi = i => i |> string_of_int |> arg;
  let parens = xs => "(" ++ (xs |> String.concat(", ")) ++ ")";
  let brackets = x => "{ " ++ x ++ " }";
  let array = xs => "[" ++ (xs |> String.concat(", ")) ++ "]";
  let funCall = (~args, name) =>
    name ++ "(" ++ (args |> String.concat(", ")) ++ ")";
  let funDef = (~args, ~mkBody, functionName) => {
    let (params, vals) = List.split(args);
    let decl =
      "function "
      ++ functionName
      ++ (params |> parens)
      ++ " "
      ++ (vals |> mkBody |> brackets);
    functionName == "" ? parens([decl]) : decl;
  };
};

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
    | CodeItem.Unit
    | Identity => s

    | OptionalArgument(c) => apply(~converter=c, ~toJS, s)

    | Option(c) =>
      let nullableToOption = x =>
        Emit.parens([x ++ " === null ? undefined : " ++ x]);
      let optionToNullable = x => x;
      let convertedArg = apply(~converter=c, ~toJS, s);
      toJS ? optionToNullable(convertedArg) : nullableToOption(convertedArg);

    | Fn((args, resultConverter))
        when
          args
          |> List.for_all(((_label, argConverter)) =>
               argConverter == CodeItem.Identity
             ) =>
      s |> apply(~converter=resultConverter, ~toJS)

    | Fn((args, resultConverter)) =>
      let resultVar = "result";
      let mkReturn = x =>
        "const "
        ++ resultVar
        ++ " = "
        ++ x
        ++ "; return "
        ++ apply(~converter=resultConverter, ~toJS, resultVar);
      let convertedArgs = {
        let convertArg = (i, (lbl, argConverter)) => {
          let varName =
            switch (lbl) {
            | NamedArgs.Nolabel => Emit.argi(i + 1)
            | Label(l)
            | OptLabel(l) => Emit.arg(l)
            };
          let notToJS = !toJS;
          (
            varName,
            varName |> apply(~converter=argConverter, ~toJS=notToJS),
          );
        };
        args |> List.mapi(convertArg);
      };
      let mkBody = args => s |> Emit.funCall(~args) |> mkReturn;
      Emit.funDef(~args=convertedArgs, ~mkBody, "");
    };
};

type env = {
  requires: StringMap.t(string),
  typeMap: StringMap.t(Flow.typ),
  exports: list((string, option(Flow.typ))),
};

let requireModule = (~env, moduleName) => {
  let requires =
    env.requires
    |> StringMap.add(moduleName, resolveSourceModule(moduleName));
  (requires, moduleName);
};

let emitCodeItems = codeItems => {
  let requireBuffer = Buffer.create(100);
  let mainBuffer = Buffer.create(100);
  let exportBuffer = Buffer.create(100);
  let line_ = (buffer, s) => {
    if (Buffer.length(buffer) > 0) {
      Buffer.add_string(buffer, "\n");
    };
    Buffer.add_string(buffer, s);
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

    | ValueBinding(inputModuleName, id, converter) =>
      let (requires, moduleStr) = inputModuleName |> requireModule(~env);
      line(
        "const "
        ++ id
        ++ " = "
        ++ (moduleStr ++ "." ++ id |> Convert.apply(~converter, ~toJS=true))
        ++ ";",
      );
      let flowType = env.typeMap |> StringMap.find(id);
      {...env, exports: [(id, Some(flowType)), ...env.exports], requires};

    | ConstructorBinding(
        constructorFlowType,
        convertableFlowTypes,
        _modulePath,
        leafName,
        runtimeValue_,
      ) =>
      let createBucklescriptBlock = "CreateBucklescriptBlock";
      let runtimeValue = string_of_int(runtimeValue_);
      if (convertableFlowTypes == []) {
        line("const " ++ leafName ++ " = " ++ runtimeValue ++ ";");
      } else {
        let args =
          convertableFlowTypes
          |> List.mapi((i, (converter, flowTyp)) => {
               let arg = Emit.argi(i + 1);
               let v = arg |> Convert.apply(~converter, ~toJS=false);
               (arg, v);
             });
        let mkBody = args =>
          createBucklescriptBlock
          ++ ".__"
          |> Emit.funCall(~args=[runtimeValue, Emit.array(args)]);
        line(leafName |> Emit.funDef(~args, ~mkBody));
      };
      {
        ...env,
        requires:
          env.requires
          |> StringMap.add(
               "CreateBucklescriptBlock",
               "bs-platform/lib/js/block.js",
             ),
        exports: [(leafName, Some(constructorFlowType)), ...env.exports],
      };

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
        ++ Emit.parens(args)
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
      {requires: StringMap.empty, typeMap: StringMap.empty, exports: []},
      codeItems,
    );

  requires |> StringMap.iter(emitRequire);

  exports |> List.rev |> List.iter(emitExport);

  [
    requireBuffer |> Buffer.to_bytes,
    mainBuffer |> Buffer.to_bytes,
    exportBuffer |> Buffer.to_bytes,
  ]
  |> String.concat("\n\n");
};