open GenTypeCommon;

type typeMap = StringMap.t((list(string), typ));

type env = {
  requiresEarly: ModuleNameMap.t(ImportPath.t),
  requires: ModuleNameMap.t(ImportPath.t),
  wrapJsComponentDeprecated: list(CodeItem.wrapJsComponentDeprecated),
  /* For each .cmt we import types from, keep the map of exported types. */
  cmtExportTypeMapCache: StringMap.t(typeMap),
  /* Map of types imported from other files. */
  typesFromOtherFiles: typeMap,
};

let requireModule = (~requires, ~importPath, moduleName) =>
  requires
  |> ModuleNameMap.add(
       moduleName,
       moduleName |> ModuleResolver.resolveSourceModule(~importPath),
     );

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
    | ValueBinding(_)
    | ComponentBinding(_)
    | ImportType(_)
    | ExportVariantType(_)
    | ConstructorBinding(_)
    | WrapJsComponentDeprecated(_)
    | WrapJsComponent(_)
    | WrapJsValue(_) => exportTypeMap
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

let emitCheckJsWrapperType =
    (
      ~emitters,
      ~config,
      ~env,
      ~propsTypeName,
      ~exportType: CodeItem.exportType,
    ) =>
  switch (env.wrapJsComponentDeprecated) {
  | [] => None

  | [{componentName, _}] =>
    let s =
      "("
      ++ (
        "props"
        |> EmitTyp.ofType(
             ~language=config.language,
             ~typ=Ident(propsTypeName, []),
           )
      )
      ++ ") {\n      return <"
      ++ componentName
      ++ " {...props}/>;\n    }";
    let exportTypeNoChildren =
      switch (exportType.typ) {
      | GroupOfLabeledArgs(fields) =>
        switch (fields |> List.rev) {
        | [_child, ...propFieldsRev] =>
          let typNoChildren = GroupOfLabeledArgs(propFieldsRev |> List.rev);
          {...exportType, typ: typNoChildren};
        | [] => exportType
        }
      | _ => exportType
      };
    let emitters =
      emitExportType(
        ~language=config.language,
        ~emitters,
        exportTypeNoChildren,
      );
    let emitters =
      EmitTyp.emitExportFunction(
        ~early=false,
        ~emitters,
        ~name="checkJsWrapperType",
        ~config,
        s,
      );

    Some(emitters);

  | [_, ..._] =>
    Some(
      "// genType warning: found more than one external component annotated with @genType"
      |> Emitters.export(~emitters),
    )
  };

let emitCodeItem =
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

  | ValueBinding({moduleName, id, typ}) =>
    let importPath =
      ModuleResolver.resolveModule(
        ~config,
        ~outputFileRelative,
        ~resolver,
        ~importExtension=".bs",
        moduleName,
      );
    let moduleNameBs = moduleName |> ModuleName.forBsFile;
    let requires =
      moduleNameBs |> requireModule(~requires=env.requires, ~importPath);
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

    ({...env, requires}, emitters);

  | ConstructorBinding(
      exportType,
      constructorType,
      argTypes,
      variantName,
      recordValue,
    ) =>
    let emitters = emitExportType(~emitters, ~language, exportType);

    let recordAsInt = recordValue |> Runtime.emitRecordAsInt(~language);
    let emitters =
      if (argTypes == []) {
        recordAsInt
        ++ ";"
        |> EmitTyp.emitExportConst(
             ~emitters,
             ~name=variantName,
             ~typ=constructorType,
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
             ~typ=constructorType,
             ~config,
           );
      };
    let newEnv = {
      ...env,
      requires:
        env.requires
        |> ModuleNameMap.add(
             ModuleName.createBucklescriptBlock,
             ImportPath.bsBlockPath(~config),
           ),
    };
    (newEnv, emitters);

  | ComponentBinding({
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

    switch (
      emitCheckJsWrapperType(
        ~emitters,
        ~config,
        ~env,
        ~propsTypeName,
        ~exportType,
      )
    ) {
    | Some(emitters) => (env, emitters)
    | None =>
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

      let requiresWithModule =
        moduleNameBs |> requireModule(~requires=env.requires, ~importPath);
      let requiresWithReasonReact =
        requiresWithModule
        |> ModuleNameMap.add(
             ModuleName.reasonReact,
             ImportPath.reasonReactPath(~config),
           );
      ({...env, requires: requiresWithReasonReact}, emitters);
    };

  | WrapJsComponentDeprecated(
      {componentName, importPath} as wrapJsComponentDeprecated,
    ) =>
    let requires =
      env.requires
      |> ModuleNameMap.add(
           ModuleName.fromStringUnsafe(componentName),
           importPath,
         );
    let newEnv = {
      ...env,
      requires,
      wrapJsComponentDeprecated: [
        wrapJsComponentDeprecated,
        ...env.wrapJsComponentDeprecated,
      ],
    };
    (newEnv, emitters);

  | WrapJsValue({valueName, importString, typ, moduleName}) =>
    let importPath = importString |> ImportPath.fromStringUnsafe;

    let (emitters, importedAsName, requiresEarly) =
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
        (emitters, valueNameNotChecked, env.requiresEarly);
      | Flow
      | Untyped =>
        /* add an early require(...)  */
        let importFile = importString |> Filename.basename;
        let importedAsName = importFile ++ "." ++ valueName;
        let requiresEarly =
          importFile
          |> ModuleName.fromStringUnsafe
          |> requireModule(~requires=env.requiresEarly, ~importPath);
        (emitters, importedAsName, requiresEarly);
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
             ++ importString
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
    let newEnv = {...env, requiresEarly};
    (newEnv, emitters);

  | WrapJsComponent({
      exportType,
      importString,
      childrenTyp,
      propsFields,
      propsTypeName,
      moduleName,
    }) =>
    let importPath = importString |> ImportPath.fromStringUnsafe;
    let componentName =
      importString |> Filename.basename |> Filename.chop_extension;

    let (emitters, requiresEarly) =
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
        (emitters, env.requiresEarly);
      | Flow
      | Untyped =>
        /* add an early require(...)  */
        let requiresEarly =
          componentName
          |> ModuleName.fromStringUnsafe
          |> requireModule(~requires=env.requiresEarly, ~importPath);
        (emitters, requiresEarly);
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
             ++ importString
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
    let requiresEarlyWithReasonReact =
      requiresEarly
      |> ModuleNameMap.add(
           ModuleName.reasonReact,
           ImportPath.reasonReactPath(~config),
         );

    let newEnv = {...env, requiresEarly: requiresEarlyWithReasonReact};
    (newEnv, emitters);
  };
};

let emitCodeItems =
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
    wrapJsComponentDeprecated: [],
    cmtExportTypeMapCache: StringMap.empty,
    typesFromOtherFiles: StringMap.empty,
  };
  let exportTypeMap = codeItems |> createExportTypeMap(~language);
  let (finalEnv, emitters) =
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
         (initialEnv, Emitters.initial),
       );
  let emitters =
    ModuleNameMap.fold(
      (moduleName, importPath, emitters) =>
        importPath
        |> EmitTyp.emitRequire(~early=true, ~emitters, ~language, ~moduleName),
      finalEnv.requiresEarly,
      emitters,
    );
  let emitters =
    finalEnv.wrapJsComponentDeprecated != [] ?
      EmitTyp.emitRequireReact(~early=false, ~emitters, ~language) : emitters;
  let emitters =
    ModuleNameMap.fold(
      (moduleName, importPath, emitters) =>
        EmitTyp.emitRequire(
          ~early=false,
          ~emitters,
          ~language,
          ~moduleName,
          importPath,
        ),
      finalEnv.requires,
      emitters,
    );
  emitters |> Emitters.toString(~separator="\n\n");
};