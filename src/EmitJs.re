open GenFlowCommon;

type env = {
  requires: ModuleNameMap.t(ImportPath.t),
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

let emitExportType = ({CodeItem.opaque, typeParams, typeName, flowType}) =>
  "export"
  ++ (opaque ? " opaque " : " ")
  ++ "type "
  ++ typeName
  ++ Flow.genericsString(List.map(Flow.render, typeParams))
  ++ " = "
  ++ Flow.render(flowType)
  ++ (opaque ? " // Reason type already checked. Making it opaque" : "");

let emitExportUnionType = ({CodeItem.typeParams, leafTypes, name}) =>
  "export type "
  ++ name
  ++ Flow.genericsString(List.map(Flow.render, typeParams))
  ++ " =\n  | "
  ++ String.concat("\n  | ", List.map(Flow.render, leafTypes));

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
  let emitRequire = (moduleName, importPath) =>
    line_(
      requireBuffer,
      "var "
      ++ ModuleName.toString(moduleName)
      ++ " = require(\""
      ++ (importPath |> ImportPath.toString)
      ++ "\");",
    );
  let emitCodeItem = (env, codeItem) =>
    switch (codeItem) {
    | CodeItem.ImportType(import) =>
      line(CodeItem.importToString(import) ++ ";");
      env;

    | CodeItem.ExportType(exportType) =>
      line(emitExportType(exportType) ++ ";");
      env;

    | CodeItem.ExportUnionType(exportUnionType) =>
      line(emitExportUnionType(exportUnionType) ++ ";");
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
          |> List.mapi((i, (converter, _flowTyp)) => {
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
               ModuleName.createBucklescriptBlock,
               ImportPath.bsPlatformBlock,
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
        | Some(_flowType) =>
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
      line("  (function (" ++ jsProps ++ ": " ++ propsTypeName ++ ") {");
      line(
        "     return "
        ++ ModuleName.toString(moduleName)
        ++ "."
        ++ Ident.name(id)
        ++ Emit.parens(args)
        ++ ";",
      );
      line("  }));");
      let requires =
        moduleName |> requireModule(~env, ~outputFileRelative, ~resolver);
      {
        ...env,
        exports: [(name, componentType), ...env.exports],
        requires:
          requires
          |> ModuleNameMap.add(ModuleName.reasonReact, ImportPath.reasonReact),
      };
    };
  let {requires, exports, _} =
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