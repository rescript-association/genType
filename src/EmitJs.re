open GenFlowCommon;

type env = {
  requires: ModuleNameMap.t(ImportPath.t),
  typeMap: StringMap.t(Flow.typ),
  exports: list((string, option(Flow.typ))),
  externalReactClass: list(CodeItem.externalReactClass),
};

let requireModule = (~requires, ~outputFileRelative, ~resolver, moduleName) =>
  requires
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
  ++ Flow.genericsString(List.map(Flow.toString, typeParams))
  ++ " = "
  ++ Flow.toString(flowType)
  ++ (opaque ? " // Reason type already checked. Making it opaque" : "");

let emitExportUnionType = ({CodeItem.typeParams, leafTypes, name}) =>
  "export type "
  ++ name
  ++ Flow.genericsString(List.map(Flow.toString, typeParams))
  ++ " =\n  | "
  ++ String.concat("\n  | ", List.map(Flow.toString, leafTypes));

let emitImport = import =>
  switch (import) {
  | CodeItem.ImportComment(s) => s
  | ImportAsFrom(typeName, asTypeName, importPath) =>
    "import type {"
    ++ typeName
    ++ (
      switch (asTypeName) {
      | Some(asT) => " as " ++ asT
      | None => ""
      }
    )
    ++ "} from '"
    ++ (importPath |> ImportPath.toString)
    ++ "'"
  };

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
      | Some(flowType) => "(" ++ s ++ ": " ++ Flow.toString(flowType) ++ ")"
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
  let emitCheckJsWrapperType = (~env, ~propsTypeName) =>
    switch (env.externalReactClass) {
    | [] => env.requires

    | [{componentName}] =>
      let s =
        "function checkJsWrapperType(props: "
        ++ propsTypeName
        ++ ") {
      return <"
        ++ componentName
        ++ " {...props}> </"
        ++ componentName
        ++ ">;
    }";
      line(s);
      env.requires |> ModuleNameMap.add(ModuleName.react, ImportPath.reactjs);

    | [_, ..._] =>
      line(
        "// genFlow warning: found more than one external component annotated with @genFlow",
      );
      env.requires;
    };
  let emitCodeItem = (env, codeItem) =>
    switch (codeItem) {
    | CodeItem.ImportType(import) =>
      line(emitImport(import) ++ ";");
      env;

    | CodeItem.ExportType(exportType) =>
      line(emitExportType(exportType) ++ ";");
      env;

    | CodeItem.ExportUnionType(exportUnionType) =>
      line(emitExportUnionType(exportUnionType) ++ ";");
      env;

    | ValueBinding(moduleName, id, flowType, converter) =>
      let requires =
        moduleName
        |> requireModule(
             ~requires=env.requires,
             ~outputFileRelative,
             ~resolver,
           );
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
      {
        ...env,
        typeMap: env.typeMap |> StringMap.add(id, flowType),
        exports: [(id, Some(flowType)), ...env.exports],
        requires,
      };

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

    | ComponentBinding({moduleName, componentType, propsTypeName, converter}) =>
      let name = "component";
      let jsProps = "jsProps";
      let jsPropsDot = s => jsProps ++ "." ++ s;
      let componentType =
        Some(
          Flow.Ident(
            "React$ComponentType",
            [Flow.Ident(propsTypeName, [])],
          ),
        );
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
        ++ "make"
        ++ Emit.parens(args)
        ++ ";",
      );
      line("  }));");

      let requiresWithCheckJsWrapper =
        emitCheckJsWrapperType(~env, ~propsTypeName);

      let requiresWithModule =
        moduleName
        |> requireModule(
             ~requires=requiresWithCheckJsWrapper,
             ~outputFileRelative,
             ~resolver,
           );
      let requiresWithReasonReact =
        requiresWithModule
        |> ModuleNameMap.add(ModuleName.reasonReact, ImportPath.reasonReact);
      {
        ...env,
        typeMap:
          switch (componentType) {
          | None => env.typeMap
          | Some(flowType) =>
            env.typeMap |> StringMap.add("component", flowType)
          },
        exports: [(name, componentType), ...env.exports],
        requires: requiresWithReasonReact,
      };

    | ExternalReactClass({componentName, importPath} as externalReactClass) =>
      line(
        "const "
        ++ componentName
        ++ " = require(\""
        ++ (importPath |> ImportPath.toString)
        ++ "\");"
        ++ " // external "
        ++ componentName
        ++ " : ReasonReact.reactClass = \""
        ++ (importPath |> ImportPath.toString)
        ++ "\"",
      );
      {
        ...env,
        externalReactClass: [externalReactClass, ...env.externalReactClass],
      };
    };
  let {requires, exports, _} =
    List.fold_left(
      emitCodeItem,
      {
        requires: ModuleNameMap.empty,
        typeMap: StringMap.empty,
        exports: [],
        externalReactClass: [],
      },
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