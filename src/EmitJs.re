open GenFlowCommon;

type env = {
  requires: ModuleNameMap.t(ImportPath.t),
  typeMap: StringMap.t(typ),
  externalReactClass: list(CodeItem.externalReactClass),
};

let requireModule = (~requires, ~importPath, moduleName) =>
  requires
  |> ModuleNameMap.add(
       moduleName,
       moduleName |> ModuleResolver.resolveSourceModule(~importPath),
     );

let emitCodeItems = (~language, ~outputFileRelative, ~resolver, codeItems) => {
  let requireBuffer = Buffer.create(100);
  let importTypeBuffer = Buffer.create(100);
  let exportBuffer = Buffer.create(100);
  let line__ = (buffer, s) => {
    if (Buffer.length(buffer) > 0) {
      Buffer.add_string(buffer, "\n");
    };
    Buffer.add_string(buffer, s);
  };
  let require = line__(requireBuffer);
  let export = line__(exportBuffer);

  let emitImportType = (~language, importType) =>
    (
      switch (importType) {
      | CodeItem.ImportComment(s) => s
      | ImportAsFrom(typeName, asTypeName, importPath) =>
        EmitTyp.emitImportTypeAs(
          ~language,
          ~typeName,
          ~asTypeName,
          ~importPath,
        )
      }
    )
    |> line__(importTypeBuffer);

  let emitExportType =
      (~language, {CodeItem.opaque, typeParams, typeName, comment, typ}) =>
    typ
    |> EmitTyp.emitExportType(
         ~language,
         ~opaque,
         ~typeName,
         ~typeParams,
         ~comment,
       )
    |> export;

  let emitCheckJsWrapperType = (~env, ~propsTypeName) =>
    switch (env.externalReactClass) {
    | [] => ()

    | [{componentName}] =>
      let s =
        "export function checkJsWrapperType("
        ++ (
          "props" |> EmitTyp.ofType(~language, ~typ=Ident(propsTypeName, []))
        )
        ++ ") {\n      return <"
        ++ componentName
        ++ " {...props}/>;\n    }";
      export(s);

    | [_, ..._] =>
      export(
        "// genFlow warning: found more than one external component annotated with @genFlow",
      )
    };

  let emitCodeItem = (env, codeItem) => {
    if (Debug.codeItems) {
      logItem("Code Item: %s\n", codeItem |> CodeItem.toString(~language));
    };
    switch (codeItem) {
    | CodeItem.ImportType(importType) =>
      emitImportType(~language, importType);
      env;

    | ExportType(exportType) =>
      emitExportType(~language, exportType);
      env;

    | ExportVariantType({CodeItem.typeParams, leafTypes, name}) =>
      EmitTyp.emitExportVariantType(~language, ~name, ~typeParams, ~leafTypes)
      |> export;
      env;

    | ValueBinding({moduleName, id, typ, converter}) =>
      let importPath =
        ModuleResolver.resolveModule(
          ~outputFileRelative,
          ~resolver,
          ~importExtension=".bs",
          moduleName,
        );
      let moduleNameBs = moduleName |> ModuleName.forBsFile;
      let requires =
        moduleNameBs |> requireModule(~requires=env.requires, ~importPath);

      "export const "
      ++ (id |> Ident.name |> EmitTyp.ofType(~language, ~typ))
      ++ " = "
      ++ (
        ModuleName.toString(moduleNameBs)
        ++ "."
        ++ Ident.name(id)
        |> Convert.toJS(~converter)
      )
      ++ ";"
      |> export;
      {...env, requires};

    | ConstructorBinding(
        exportType,
        constructorType,
        convertableTypes,
        variantName,
        recordValue,
      ) =>
      emitExportType(~language, exportType);
      let recordAsInt = recordValue |> Runtime.emitRecordAsInt(~language);
      if (convertableTypes == []) {
        "export const "
        ++ (variantName |> EmitTyp.ofType(~language, ~typ=constructorType))
        ++ " = "
        ++ recordAsInt
        ++ ";"
        |> export;
      } else {
        let args =
          convertableTypes
          |> List.mapi((i, (converter, _flowTyp)) => {
               let arg = Emit.argi(i + 1);
               let v = arg |> Convert.toReason(~converter);
               (arg, v);
             });
        let mkReturn = s => "return " ++ s;
        let mkBody = args =>
          recordValue
          |> Runtime.emitRecordAsBlock(~language, ~args)
          |> mkReturn;
        "export const "
        ++ (variantName |> EmitTyp.ofType(~language, ~typ=constructorType))
        ++ " = "
        ++ Emit.funDef(~args, ~mkBody, "")
        |> export;
      };
      {
        ...env,
        requires:
          env.requires
          |> ModuleNameMap.add(
               ModuleName.createBucklescriptBlock,
               ImportPath.bsPlatformBlock,
             ),
      };

    | ComponentBinding({
        exportType,
        moduleName,
        propsTypeName,
        componentType,
        converter,
      }) =>
      let importPath =
        ModuleResolver.resolveModule(
          ~outputFileRelative,
          ~resolver,
          ~importExtension=".bs",
          moduleName,
        );
      let moduleNameBs = moduleName |> ModuleName.forBsFile;

      let name = EmitTyp.componentExportName(~language, ~moduleName);
      let jsProps = "jsProps";
      let jsPropsDot = s => jsProps ++ "." ++ s;
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
              |> List.map(((s, argConverter)) =>
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

      emitExportType(~language, exportType);
      export(
        "export const "
        ++ (name |> EmitTyp.ofType(~language, ~typ=componentType))
        ++ " = ReasonReact.wrapReasonForJs(",
      );

      export(
        "  " ++ ModuleName.toString(moduleNameBs) ++ ".component" ++ ",",
      );
      export("  (function _(" ++ jsProps ++ ": " ++ propsTypeName ++ ") {");
      export(
        "     return "
        ++ ModuleName.toString(moduleNameBs)
        ++ "."
        ++ "make"
        ++ Emit.parens(args)
        ++ ";",
      );
      export("  }));");

      emitCheckJsWrapperType(~env, ~propsTypeName);

      let requiresWithModule =
        moduleNameBs |> requireModule(~requires=env.requires, ~importPath);
      let requiresWithReasonReact =
        requiresWithModule
        |> ModuleNameMap.add(ModuleName.reasonReact, ImportPath.reasonReact);
      {...env, requires: requiresWithReasonReact};

    | ExternalReactClass({componentName, importPath} as externalReactClass) =>
      let requires =
        env.requires
        |> ModuleNameMap.add(
             ModuleName.fromStringUnsafe(componentName),
             importPath,
           );
      {
        ...env,
        requires,
        externalReactClass: [externalReactClass, ...env.externalReactClass],
      };
    };
  };

  let updateTypeMap = (env, codeItem) => {
    let addType = (name, typ) => {
      if (Debug.codeItems) {
        logItem(
          "Add Type: %s = %s\n",
          name,
          typ |> EmitTyp.typToString(~language),
        );
      };
      {...env, typeMap: env.typeMap |> StringMap.add(name, typ)};
    };
    switch (codeItem) {
    | CodeItem.ValueBinding({id, typ}) => addType(id |> Ident.name, typ)
    | ComponentBinding({componentType}) =>
      addType("component", componentType)
    | ImportType(_)
    | ExportType(_)
    | ExportVariantType(_)
    | ConstructorBinding(_)
    | ExternalReactClass(_) => env
    };
  };

  let initialEnv = {
    requires: ModuleNameMap.empty,
    typeMap: StringMap.empty,
    externalReactClass: [],
  };
  let envWithTypeMaps = List.fold_left(updateTypeMap, initialEnv, codeItems);
  let finalEnv = List.fold_left(emitCodeItem, envWithTypeMaps, codeItems);

  if (finalEnv.externalReactClass != []) {
    EmitTyp.requireReact(~language) |> require;
  };
  finalEnv.requires
  |> ModuleNameMap.iter((moduleName, importPath) =>
       EmitTyp.emitRequire(~language, moduleName, importPath) |> require
     );

  let requireString = requireBuffer |> Buffer.to_bytes;
  let importTypeString = importTypeBuffer |> Buffer.to_bytes;
  let exportString = exportBuffer |> Buffer.to_bytes;
  let toList = s => s == "" ? [] : [s];

  (requireString |> toList)
  @ (importTypeString |> toList)
  @ (exportString |> toList)
  |> String.concat("\n\n");
};