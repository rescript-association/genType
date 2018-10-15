open GenTypeCommon;

type typeMap = StringMap.t((list(string), typ));

type env = {
  requiresEarly: ModuleNameMap.t((ImportPath.t, bool)),
  requires: ModuleNameMap.t((ImportPath.t, bool)),
  /* For each .cmt we import types from, keep the map of exported types. */
  cmtExportTypeMapCache: StringMap.t(typeMap),
  /* Map of types imported from other files. */
  typesFromOtherFiles: typeMap,
};

let requireModule = (~early, ~env, ~importPath, ~strict=false, moduleName) => {
  let requires = early ? env.requiresEarly : env.requires;
  let requiresNew =
    requires
    |> ModuleNameMap.add(
         moduleName,
         (
           moduleName |> ModuleResolver.resolveSourceModule(~importPath),
           strict,
         ),
       );
  early ?
    {...env, requiresEarly: requiresNew} : {...env, requires: requiresNew};
};

let createExportTypeMap = (~language, codeItems): typeMap => {
  let updateExportTypeMap = (exportTypeMap: typeMap, codeItem): typeMap => {
    let addExportType = ({typeName, typeVars, typ, _}: CodeItem.exportType) => {
      if (Debug.codeItems) {
        logItem(
          "Export Type: %s%s = %s\n",
          typeName,
          typeVars == [] ?
            "" : "(" ++ (typeVars |> String.concat(",")) ++ ")",
          typ |> EmitTyp.typToString(~language),
        );
      };
      exportTypeMap |> StringMap.add(typeName, (typeVars, typ));
    };
    switch (codeItem) {
    | CodeItem.ExportType(exportType) => exportType |> addExportType
    | ImportType(_)
    | ExportVariantType(_)
    | WrapJsComponent(_)
    | WrapJsValue(_)
    | WrapModule(_)
    | WrapReasonComponent(_)
    | WrapReasonValue(_)
    | WrapVariant(_) => exportTypeMap
    };
  };
  codeItems |> List.fold_left(updateExportTypeMap, StringMap.empty);
};

let emitImportType =
    (~language, ~emitters, ~inputCmtToTypeDeclarations, ~env, importType) =>
  switch (importType) {
  | CodeItem.ImportComment(s) => (env, s |> Emitters.import(~emitters))
  | ImportTypeAs({typeName, asTypeName, importPath, cmtFile}) =>
    let emitters =
      EmitTyp.emitImportTypeAs(
        ~emitters,
        ~language,
        ~typeName,
        ~asTypeName,
        ~importPath,
      );

    switch (asTypeName, cmtFile) {
    | (None, _)
    | (_, None) => (env, emitters)
    | (Some(asType), Some(cmtFile)) =>
      let updateTypeMapFromOtherFiles = (~exportTypeMapFromCmt) =>
        switch (exportTypeMapFromCmt |> StringMap.find(typeName)) {
        | x => env.typesFromOtherFiles |> StringMap.add(asType, x)
        | exception Not_found => exportTypeMapFromCmt
        };
      switch (env.cmtExportTypeMapCache |> StringMap.find(cmtFile)) {
      | exportTypeMapFromCmt => (
          {
            ...env,
            typesFromOtherFiles:
              updateTypeMapFromOtherFiles(~exportTypeMapFromCmt),
          },
          emitters,
        )
      | exception Not_found =>
        let exportTypeMapFromCmt =
          Cmt_format.read_cmt(cmtFile)
          |> inputCmtToTypeDeclarations(~language)
          |> createExportTypeMap(~language);
        let cmtExportTypeMapCache =
          env.cmtExportTypeMapCache
          |> StringMap.add(cmtFile, exportTypeMapFromCmt);
        (
          {
            ...env,
            cmtExportTypeMapCache,
            typesFromOtherFiles:
              updateTypeMapFromOtherFiles(~exportTypeMapFromCmt),
          },
          emitters,
        );
      };
    };
  };

let emitExportType =
    (
      ~early=false,
      ~emitters,
      ~language,
      {CodeItem.opaque, typeVars, typeName, comment, typ},
    ) =>
  typ
  |> EmitTyp.emitExportType(
       ~early,
       ~emitters,
       ~language,
       ~opaque,
       ~typeName,
       ~typeVars,
       ~comment,
     );

let rec emitCodeItem =
        (
          ~config,
          ~outputFileRelative,
          ~resolver,
          ~exportTypeMap,
          ~emitters,
          ~env,
          ~inputCmtToTypeDeclarations,
          codeItem,
        ) => {
  let language = config.language;
  let typToConverter = typ =>
    typ
    |> Converter.typToConverter(
         ~language,
         ~exportTypeMap,
         ~typesFromOtherFiles=env.typesFromOtherFiles,
       );
  if (Debug.codeItems) {
    logItem("Code Item: %s\n", codeItem |> CodeItem.toString(~language));
  };

  switch (codeItem) {
  | CodeItem.ImportType(importType) =>
    emitImportType(
      ~language,
      ~emitters,
      ~inputCmtToTypeDeclarations,
      ~env,
      importType,
    )

  | ExportType(exportType) => (
      env,
      emitExportType(~emitters, ~language, exportType),
    )

  | ExportVariantType({CodeItem.typeParams, leafTypes, name}) => (
      env,
      EmitTyp.emitExportVariantType(
        ~emitters,
        ~language,
        ~name,
        ~typeParams,
        ~leafTypes,
      ),
    )

  | WrapReasonValue({moduleName, id, typ}) =>
    let importPath =
      ModuleResolver.resolveModule(
        ~config,
        ~outputFileRelative,
        ~resolver,
        ~importExtension=".bs",
        moduleName,
      );
    let moduleNameBs = moduleName |> ModuleName.forBsFile;
    let envWithRequires =
      moduleNameBs |> requireModule(~early=false, ~env, ~importPath);
    let converter = typ |> typToConverter;

    let emitters =
      (
        ModuleName.toString(moduleNameBs)
        ++ "."
        ++ Ident.name(id)
        |> Converter.toJS(~converter)
      )
      ++ ";"
      |> EmitTyp.emitExportConst(
           ~emitters,
           ~name=id |> Ident.name,
           ~typ,
           ~config,
         );

    (envWithRequires, emitters);

  | WrapVariant({
      exportType,
      constructorTyp,
      argTypes,
      variantName,
      recordValue,
    }) =>
    let emitters = emitExportType(~emitters, ~language, exportType);

    let recordAsInt = recordValue |> Runtime.emitRecordAsInt(~language);
    let emitters =
      if (argTypes == []) {
        recordAsInt
        ++ ";"
        |> EmitTyp.emitExportConst(
             ~emitters,
             ~name=variantName,
             ~typ=constructorTyp,
             ~config,
           );
      } else {
        let args =
          argTypes
          |> List.mapi((i, typ) => {
               let converter = typ |> typToConverter;
               let arg = EmitText.argi(i + 1);
               let v = arg |> Converter.toReason(~converter);
               (arg, v);
             });
        let mkReturn = s => "return " ++ s;
        let mkBody = args =>
          recordValue
          |> Runtime.emitRecordAsBlock(~language, ~args)
          |> mkReturn;
        EmitText.funDef(~args, ~mkBody, "")
        |> EmitTyp.emitExportConst(
             ~emitters,
             ~name=variantName,
             ~typ=constructorTyp,
             ~config,
           );
      };
    let env =
      ModuleName.createBucklescriptBlock
      |> requireModule(
           ~early=false,
           ~env,
           ~importPath=ImportPath.bsBlockPath(~config),
         );
    (env, emitters);

  | WrapReasonComponent({
      exportType,
      moduleName,
      propsTypeName,
      componentType,
      typ,
    }) =>
    let converter = typ |> typToConverter;
    let importPath =
      ModuleResolver.resolveModule(
        ~config,
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
      | FunctionC((groupedArgConverters, _retConverter)) =>
        switch (groupedArgConverters) {
        | [
            GroupConverter(propConverters),
            ArgConverter(_, childrenConverter),
            ..._,
          ] =>
          (
            propConverters
            |> List.map(((s, argConverter)) =>
                 jsPropsDot(s) |> Converter.toReason(~converter=argConverter)
               )
          )
          @ [
            jsPropsDot("children")
            |> Converter.toReason(~converter=childrenConverter),
          ]

        | [ArgConverter(_, childrenConverter), ..._] => [
            jsPropsDot("children")
            |> Converter.toReason(~converter=childrenConverter),
          ]

        | _ => [jsPropsDot("children")]
        }

      | _ => [jsPropsDot("children")]
      };

    let emitters = emitExportType(~emitters, ~language, exportType);
    let emitters =
      EmitTyp.emitExportConstMany(
        ~emitters,
        ~name,
        ~typ=componentType,
        ~config,
        [
          "ReasonReact.wrapReasonForJs(",
          "  " ++ ModuleName.toString(moduleNameBs) ++ ".component" ++ ",",
          "  (function _("
          ++ EmitTyp.ofType(
               ~language,
               ~typ=Ident(propsTypeName, []),
               jsProps,
             )
          ++ ") {",
          "     return "
          ++ ModuleName.toString(moduleNameBs)
          ++ "."
          ++ "make"
          ++ EmitText.parens(args)
          ++ ";",
          "  }));",
        ],
      );

    let emitters = EmitTyp.emitExportDefault(~emitters, ~config, name);

    let env = moduleNameBs |> requireModule(~early=false, ~env, ~importPath);

    let env =
      ModuleName.reasonReact
      |> requireModule(
           ~early=false,
           ~env,
           ~importPath=ImportPath.reasonReactPath(~config),
         );

    (env, emitters);

  | WrapJsValue({valueName, importAnnotation, typ, moduleName}) =>
    let importPath = importAnnotation.importPath;
    let (emitters, importedAsName, env) =
      switch (language) {
      | Typescript =>
        /* emit an import {... as ...} immediately */
        let valueNameNotChecked = valueName ++ "NotChecked";
        let emitters =
          importPath
          |> EmitTyp.emitImportValueAsEarly(
               ~emitters,
               ~name=valueName,
               ~nameAs=Some(valueNameNotChecked),
             );
        (emitters, valueNameNotChecked, env);
      | Flow
      | Untyped =>
        /* add an early require(...)  */
        let importFile = importAnnotation.name;

        let importedAsName = importFile ++ "." ++ valueName;
        let env =
          importFile
          |> ModuleName.fromStringUnsafe
          |> requireModule(~early=true, ~env, ~importPath, ~strict=true);
        (emitters, importedAsName, env);
      };
    let converter = typ |> typToConverter;
    let valueNameTypeChecked = valueName ++ "TypeChecked";

    let emitters =
      importedAsName
      ++ ";"
      |> EmitTyp.emitExportConstEarly(
           ~emitters,
           ~name=valueNameTypeChecked,
           ~typ,
           ~config,
           ~comment=
             "In case of type error, check the type of '"
             ++ valueName
             ++ "' in '"
             ++ (moduleName |> ModuleName.toString)
             ++ ".re'"
             ++ " and '"
             ++ (importPath |> ImportPath.toString)
             ++ "'.",
         );
    let emitters =
      (valueNameTypeChecked |> Converter.toReason(~converter))
      ++ ";"
      |> EmitTyp.emitExportConstEarly(
           ~comment=
             "Export '"
             ++ valueName
             ++ "' early to allow circular import from the '.bs.js' file.",
           ~emitters,
           ~name=valueName,
           ~typ,
           ~config,
         );
    (env, emitters);

  | WrapJsComponent({
      exportType,
      importAnnotation,
      childrenTyp,
      propsFields,
      propsTypeName,
      moduleName,
    }) =>
    let importPath = importAnnotation.importPath;
    let componentName = importAnnotation.name;

    let (emitters, env) =
      switch (language) {
      | Typescript =>
        /* emit an import {... as ...} immediately */
        let emitters =
          importPath
          |> EmitTyp.emitImportValueAsEarly(
               ~emitters,
               ~name=componentName,
               ~nameAs=None,
             );
        (emitters, env);
      | Flow
      | Untyped =>
        /* add an early require(...)  */
        let env =
          componentName
          |> ModuleName.fromStringUnsafe
          |> requireModule(~early=true, ~env, ~importPath, ~strict=true);
        (emitters, env);
      };
    let componentNameTypeChecked = componentName ++ "TypeChecked";

    /* Check the type of the component */
    let emitters =
      EmitTyp.emitRequireReact(~early=true, ~emitters, ~language);
    let emitters =
      emitExportType(
        ~early=true,
        ~language=config.language,
        ~emitters,
        exportType,
      );
    let emitters =
      "("
      ++ (
        "props"
        |> EmitTyp.ofType(
             ~language=config.language,
             ~typ=Ident(propsTypeName, []),
           )
      )
      ++ ") {\n  return <"
      ++ componentName
      ++ " {...props}/>;\n}"
      |> EmitTyp.emitExportFunction(
           ~early=true,
           ~emitters,
           ~name=componentNameTypeChecked,
           ~config,
           ~comment=
             "In case of type error, check the type of '"
             ++ "make"
             ++ "' in '"
             ++ (moduleName |> ModuleName.toString)
             ++ ".re'"
             ++ " and the props of '"
             ++ (importPath |> ImportPath.toString)
             ++ "'.",
         );

    /* Wrap the component */
    let emitters =
      (
        "function _"
        ++ EmitText.parens(
             (propsFields |> List.map(((propName, _, _)) => propName))
             @ ["children"]
             |> List.map(EmitTyp.ofTypeAny(~language)),
           )
        ++ " { return ReasonReact.wrapJsForReason"
        ++ EmitText.parens([
             componentName,
             "{"
             ++ (
               propsFields
               |> List.map(((propName, optionalness, propTyp)) =>
                    propName
                    ++ ": "
                    ++ (
                      propName
                      |> Converter.toJS(
                           ~converter=
                             (
                               optionalness == Mandatory ?
                                 propTyp : Option(propTyp)
                             )
                             |> typToConverter,
                         )
                    )
                  )
               |> String.concat(", ")
             )
             ++ "}",
             "children"
             |> Converter.toJS(~converter=childrenTyp |> typToConverter),
           ])
        ++ "; }"
      )
      ++ ";"
      |> EmitTyp.emitExportConstEarly(
           ~comment=
             "Export '"
             ++ "make"
             ++ "' early to allow circular import from the '.bs.js' file.",
           ~emitters,
           ~name="make",
           ~typ=mixedOrUnknown(~language),
           ~config,
         );
    let env =
      ModuleName.reasonReact
      |> requireModule(
           ~early=true,
           ~env,
           ~importPath=ImportPath.reasonReactPath(~config),
         );
    (env, emitters);

  | WrapModule({codeItems}) =>
    codeItems
    |> emitCodeItems(
         ~config,
         ~outputFileRelative,
         ~resolver,
         ~exportTypeMap,
         ~emitters,
         ~env,
         ~inputCmtToTypeDeclarations,
       )
  };
}
and emitCodeItems =
    (
      ~config,
      ~outputFileRelative,
      ~resolver,
      ~exportTypeMap,
      ~emitters,
      ~env,
      ~inputCmtToTypeDeclarations,
      codeItems,
    ) =>
  codeItems
  |> List.fold_left(
       ((env, emitters)) =>
         emitCodeItem(
           ~config,
           ~outputFileRelative,
           ~resolver,
           ~exportTypeMap,
           ~emitters,
           ~env,
           ~inputCmtToTypeDeclarations,
         ),
       (env, emitters),
     );

let emitRequires = (~early, ~language, ~requires, emitters) =>
  ModuleNameMap.fold(
    (moduleName, (importPath, strict), emitters) =>
      importPath
      |> EmitTyp.emitRequire(
           ~early,
           ~emitters,
           ~language,
           ~moduleName,
           ~strict,
         ),
    requires,
    emitters,
  );

let emitCodeItemsAsString =
    (
      ~config,
      ~outputFileRelative,
      ~resolver,
      ~inputCmtToTypeDeclarations,
      codeItems,
    ) => {
  let language = config.language;

  let initialEnv = {
    requires: ModuleNameMap.empty,
    requiresEarly: ModuleNameMap.empty,
    cmtExportTypeMapCache: StringMap.empty,
    typesFromOtherFiles: StringMap.empty,
  };
  let exportTypeMap = codeItems |> createExportTypeMap(~language);
  let (finalEnv, emitters) =
    codeItems
    |> emitCodeItems(
         ~config,
         ~outputFileRelative,
         ~resolver,
         ~exportTypeMap,
         ~emitters=Emitters.initial,
         ~env=initialEnv,
         ~inputCmtToTypeDeclarations,
       );

  emitters
  |> emitRequires(~early=true, ~language, ~requires=finalEnv.requiresEarly)
  |> emitRequires(~early=false, ~language, ~requires=finalEnv.requires)
  |> Emitters.toString(~separator="\n\n");
};