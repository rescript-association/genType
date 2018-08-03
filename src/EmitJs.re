open GenFlowCommon;

type env = {
  requires: ModuleNameMap.t(ImportPath.t),
  typeMap: StringMap.t(typ),
  exports: list((string, option(typ))),
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

let emitExportType = ({CodeItem.opaque, typeParams, typeName, typ}) =>
  "export"
  ++ (opaque ? " opaque " : " ")
  ++ "type "
  ++ typeName
  ++ EmitTyp.genericsString(List.map(EmitTyp.toString, typeParams))
  ++ " = "
  ++ EmitTyp.toString(typ)
  ++ (opaque ? " // Reason type already checked. Making it opaque" : "");

let emitExportUnionType = ({CodeItem.typeParams, leafTypes, name}) =>
  "export type "
  ++ name
  ++ EmitTyp.genericsString(List.map(EmitTyp.toString, typeParams))
  ++ " =\n  | "
  ++ String.concat("\n  | ", List.map(EmitTyp.toString, leafTypes));

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
  let emitExport = ((id, typeOpt)) => {
    let addType = s =>
      switch (typeOpt) {
      | None => s
      | Some(typ) => "(" ++ s ++ ": " ++ EmitTyp.toString(typ) ++ ")"
      };
    line_(exportBuffer, "exports." ++ id ++ " = " ++ addType(id) ++ ";");
  };
  let emitRequire = (moduleName, importPath) =>
    line_(
      requireBuffer,
      "const "
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
  let emitCodeItem = (env, codeItem) => {
    if (Debug.codeItems) {
      logItem("Code Item: %s\n", codeItem |> CodeItem.toString);
    };
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

    | ValueBinding(moduleName, id, typ, converter) =>
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
        typeMap: env.typeMap |> StringMap.add(id, typ),
        exports: [(id, Some(typ)), ...env.exports],
        requires,
      };

    | ConstructorBinding(
        constructorType,
        convertableTypes,
        variantName,
        recordValue,
      ) =>
      let recordAsInt = recordValue |> Runtime.emitRecordAsInt;
      if (convertableTypes == []) {
        line("const " ++ variantName ++ " = " ++ recordAsInt ++ ";");
      } else {
        let args =
          convertableTypes
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
        exports: [(variantName, Some(constructorType)), ...env.exports],
      };

    | ComponentBinding({moduleName, componentType, propsTypeName, converter}) =>
      let name = "component";
      let jsProps = "jsProps";
      let jsPropsDot = s => jsProps ++ "." ++ s;
      let componentType =
        Some(Ident("React$ComponentType", [Ident(propsTypeName, [])]));
      let args =
        switch (converter) {
        | Fn((groupedArgConverters, _retConverter)) =>
          switch (groupedArgConverters) {
          | [
              GroupConverter(propConverters),
              ArgConverter(_, childrenConverter),
              ..._,
            ] =>
            (
              propConverters
              |> List.map(((s, _optionalness, argConverter)) =>
                   jsPropsDot(s)
                   |> Convert.apply(~converter=argConverter, ~toJS=true)
                 )
            )
            @ [
              jsPropsDot("children")
              |> Convert.apply(~converter=childrenConverter, ~toJS=true),
            ]

          | [ArgConverter(_, childrenConverter), ..._] => [
              jsPropsDot("children")
              |> Convert.apply(~converter=childrenConverter, ~toJS=true),
            ]

          | _ => [jsPropsDot("children")]
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
          | Some(typ) => env.typeMap |> StringMap.add("component", typ)
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