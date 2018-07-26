open GenFlowCommon;

module ModuleNameMap = Map.Make(ModuleName);

type env = {
  requires: ModuleNameMap.t(string),
  typeMap: StringMap.t(Flow.typ),
  exports: list((string, option(Flow.typ))),
};

let requireModule = (~env, ~outputFileRelative, ~resolver, moduleName) =>
  env.requires
  |> ModuleNameMap.add(
       moduleName,
       ModuleResolver.resolveSourceModule(
         ~outputFileRelative,
         ~resolver,
         moduleName,
       ),
     );

let emitCodeItems = (~outputFileRelative, ~resolver, codeItems) => {
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
  let emitRequire = (moduleName, v) =>
    line_(
      requireBuffer,
      "var "
      ++ ModuleName.toString(moduleName)
      ++ " = require(\""
      ++ v
      ++ "\");",
    );
  let emitCodeItem = (env, codeItem) =>
    switch (codeItem) {
    | CodeItem.ImportType(import) =>
      line(CodeItem.importToString(import) ++ ";");
      env;

    | CodeItem.ExportType(exportType) =>
      line(CodeItem.exportTypeToString(exportType) ++ ";");
      env;

    | CodeItem.ExportUnionType(exportUnionType) =>
      line(CodeItem.exportUnionTypeToString(exportUnionType) ++ ";");
      env;

    | FlowTypeBinding(id, flowType) => {
        ...env,
        typeMap: env.typeMap |> StringMap.add(id, flowType),
      }

    | ValueBinding(moduleName, id, converter) =>
      let requires =
        moduleName |> requireModule(~env, ~outputFileRelative, ~resolver);
      line(
        "const "
        ++ id
        ++ " = "
        ++ (
          ModuleName.toString(moduleName)
          ++ "."
          ++ id
          |> Convert.toJS(~converter)
        )
        ++ ";",
      );
      let flowType = env.typeMap |> StringMap.find(id);
      {...env, exports: [(id, Some(flowType)), ...env.exports], requires};

    | ConstructorBinding(
        constructorFlowType,
        convertableFlowTypes,
        variantName,
        recordValue,
      ) =>
      let recordAsInt = recordValue |> Runtime.emitRecordAsInt;
      if (convertableFlowTypes == []) {
        line("const " ++ variantName ++ " = " ++ recordAsInt ++ ";");
      } else {
        let args =
          convertableFlowTypes
          |> List.mapi((i, (converter, flowTyp)) => {
               let arg = Emit.argi(i + 1);
               let v = arg |> Convert.toReason(~converter);
               (arg, v);
             });
        let mkBody = args => recordValue |> Runtime.emitRecordAsBlock(~args);
        line(variantName |> Emit.funDef(~args, ~mkBody));
      };
      {
        ...env,
        requires:
          env.requires
          |> ModuleNameMap.add(
               ModuleName.fromString("CreateBucklescriptBlock"),
               "bs-platform/lib/js/block.js",
             ),
        exports: [(variantName, Some(constructorFlowType)), ...env.exports],
      };

    | ComponentBinding(
        moduleName,
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
      line("  " ++ ModuleName.toString(moduleName) ++ ".component" ++ ",");
      line(
        "  (function (" ++ jsProps ++ ": {...Props, children:any}" ++ ") {",
      );
      line(
        "     return "
        ++ ModuleName.toString(moduleName)
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
          |> ModuleNameMap.add(
               ModuleName.fromString("ReasonReact"),
               "reason-react/src/ReasonReact.js",
             ),
      };
    };
  let {requires, exports} =
    List.fold_left(
      emitCodeItem,
      {requires: ModuleNameMap.empty, typeMap: StringMap.empty, exports: []},
      codeItems,
    );

  requires |> ModuleNameMap.iter(emitRequire);

  exports |> List.rev |> List.iter(emitExport);

  [
    requireBuffer |> Buffer.to_bytes,
    mainBuffer |> Buffer.to_bytes,
    exportBuffer |> Buffer.to_bytes,
  ]
  |> String.concat("\n\n");
};