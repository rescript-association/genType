open GenTypeCommon;

type env = {
  requiresEarly: ModuleNameMap.t((ImportPath.t, bool)),
  requires: ModuleNameMap.t((ImportPath.t, bool)),
  /* For each .cmt we import types from, keep the map of exported types. */
  cmtExportTypeMapCache: StringMap.t(Translation.typeMap),
  /* Map of types imported from other files. */
  typesFromOtherFiles: Translation.typeMap,
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

let createExportTypeMap =
    (~language, declarations: list(Translation.typeDeclaration))
    : Translation.typeMap => {
  let updateExportTypeMap =
      (
        exportTypeMap: Translation.typeMap,
        typeDeclaration: Translation.typeDeclaration,
      )
      : Translation.typeMap => {
    let addExportType =
        (
          ~importTypes,
          ~genTypeKind,
          {resolvedTypeName, typeVars, optTyp, _}: CodeItem.exportType,
        ) => {
      if (Debug.codeItems) {
        logItem(
          "Export Type: %s%s%s\n",
          resolvedTypeName,
          typeVars == [] ?
            "" : "(" ++ (typeVars |> String.concat(",")) ++ ")",
          switch (optTyp) {
          | Some(typ) =>
            " "
            ++ (genTypeKind |> genTypeKindToString |> EmitText.comment)
            ++ " = "
            ++ (typ |> EmitTyp.typToString(~language))
          | None => ""
          },
        );
      };
      switch (optTyp) {
      | Some(typ) =>
        exportTypeMap
        |> StringMap.add(
             resolvedTypeName,
             (typeVars, typ, genTypeKind, importTypes),
           )
      | None => exportTypeMap
      };
    };
    switch (typeDeclaration.exportFromTypeDeclaration) {
    | {exportKind: ExportType(exportType), genTypeKind} =>
      exportType
      |> addExportType(~genTypeKind, ~importTypes=typeDeclaration.importTypes)
    | {exportKind: ExportVariantLeaf(_) | ExportVariantType(_)} => exportTypeMap
    };
  };
  declarations |> List.fold_left(updateExportTypeMap, StringMap.empty);
};

let codeItemToString = (~language, codeItem: CodeItem.t) =>
  switch (codeItem) {
  | ExportComponent({fileName, moduleName, _}) =>
    "ExportComponent fileName:"
    ++ (fileName |> ModuleName.toString)
    ++ " moduleName:"
    ++ (moduleName |> ModuleName.toString)
  | ExportValue({fileName, resolvedName, typ, _}) =>
    "WrapReasonValue"
    ++ " resolvedName:"
    ++ resolvedName
    ++ " moduleName:"
    ++ ModuleName.toString(fileName)
    ++ " typ:"
    ++ EmitTyp.typToString(~language, typ)
  | ImportComponent({importAnnotation, _}) =>
    "ImportComponent " ++ (importAnnotation.importPath |> ImportPath.toString)
  | ImportValue({importAnnotation, _}) =>
    "ImportValue " ++ (importAnnotation.importPath |> ImportPath.toString)
  };

let emitExportType =
    (
      ~early=?,
      ~emitters,
      ~language,
      ~typIsOpaque,
      {CodeItem.opaque, typeVars, resolvedTypeName, optTyp, _},
    ) => {
  let opaque =
    switch (opaque, optTyp) {
    | (Some(opaque), _) => opaque
    | (None, Some(typ)) => typ |> typIsOpaque
    | (None, None) => false
    };
  resolvedTypeName
  |> EmitTyp.emitExportType(
       ~early?,
       ~emitters,
       ~language,
       ~opaque,
       ~typeVars,
       ~optTyp,
     );
};

let emitexportFromTypeDeclaration =
    (
      ~config,
      ~emitters,
      ~language,
      ~typIsOpaque,
      ~env,
      ~typToConverter,
      ~enumTables,
      exportFromTypeDeclaration: CodeItem.exportFromTypeDeclaration,
    ) =>
  switch (exportFromTypeDeclaration.exportKind) {
  | ExportType(exportType) => (
      env,
      emitExportType(~emitters, ~language, ~typIsOpaque, exportType),
    )

  | ExportVariantType({CodeItem.typeParams, variants, name, _}) => (
      env,
      EmitTyp.emitExportVariantType(
        ~emitters,
        ~language,
        ~name,
        ~typeParams,
        ~variants,
      ),
    )

  | ExportVariantLeaf({
      exportType,
      constructorTyp,
      argTypes,
      leafName,
      recordValue,
    }) =>
    let createFunctionType =
        ({CodeItem.typeVars, argTypes, variant: {name, params}}) => {
      let retType = Ident(name, params);
      if (argTypes === []) {
        retType;
      } else {
        Function({typeVars, argTypes, retType});
      };
    };

    let emitters =
      emitExportType(~emitters, ~language, ~typIsOpaque, exportType);

    let recordAsInt = recordValue |> Runtime.emitRecordAsInt(~language);
    let emitters =
      if (argTypes == []) {
        recordAsInt
        ++ ";"
        |> EmitTyp.emitExportConst(
             ~emitters,
             ~name=leafName,
             ~typ=constructorTyp |> createFunctionType,
             ~config,
           );
      } else {
        let args =
          argTypes
          |> List.mapi((i, typ) => {
               let converter = typ |> typToConverter;
               let arg = EmitText.argi(i + 1);
               let v = arg |> Converter.toReason(~converter, ~enumTables);
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
             ~name=leafName,
             ~typ=constructorTyp |> createFunctionType,
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
  };

let emitExportFromTypeDeclarations =
    (
      ~config,
      ~emitters,
      ~language,
      ~typIsOpaque,
      ~env,
      ~typToConverter,
      ~enumTables,
      exportFromTypeDeclarations,
    ) =>
  exportFromTypeDeclarations
  |> List.fold_left(
       ((env, emitters)) =>
         emitexportFromTypeDeclaration(
           ~config,
           ~emitters,
           ~language,
           ~typIsOpaque,
           ~env,
           ~typToConverter,
           ~enumTables,
         ),
       (env, emitters),
     );

let rec emitCodeItem =
        (
          ~config,
          ~outputFileRelative,
          ~resolver,
          ~emitters,
          ~env,
          ~enumTables,
          ~typIsOpaque,
          ~typToConverter,
          codeItem,
        ) => {
  let language = config.language;
  if (Debug.codeItems) {
    logItem("Code Item: %s\n", codeItem |> codeItemToString(~language));
  };

  switch (codeItem) {
  | ImportComponent({
      exportType,
      importAnnotation,
      childrenTyp,
      propsFields,
      propsTypeName,
      fileName,
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
        ~typIsOpaque,
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
             ++ (fileName |> ModuleName.toString)
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
             (propsFields |> List.map(({name, _}: field) => name))
             @ ["children"]
             |> List.map(EmitTyp.ofTypeAny(~language)),
           )
        ++ " { return ReasonReact.wrapJsForReason"
        ++ EmitText.parens([
             componentName,
             "{"
             ++ (
               propsFields
               |> List.map(({name: propName, optional, typ: propTyp, _}) =>
                    propName
                    ++ ": "
                    ++ (
                      propName
                      |> Converter.toJS(
                           ~converter=
                             (
                               optional == Mandatory ?
                                 propTyp : Option(propTyp)
                             )
                             |> typToConverter,
                           ~enumTables,
                         )
                    )
                  )
               |> String.concat(", ")
             )
             ++ "}",
             "children"
             |> Converter.toJS(
                  ~converter=childrenTyp |> typToConverter,
                  ~enumTables,
                ),
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

  | ImportValue({valueName, importAnnotation, typ, fileName}) =>
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
             ++ (fileName |> ModuleName.toString)
             ++ ".re'"
             ++ " and '"
             ++ (importPath |> ImportPath.toString)
             ++ "'.",
         );
    let emitters =
      (valueNameTypeChecked |> Converter.toReason(~converter, ~enumTables))
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

  | ExportComponent({
      exportType,
      fileName,
      moduleName,
      propsTypeName,
      componentType,
      typ,
    }) =>
    let converter = typ |> typToConverter;
    let importPath =
      fileName
      |> ModuleResolver.resolveModule(
           ~config,
           ~outputFileRelative,
           ~resolver,
           ~importExtension=".bs",
         );
    let moduleNameBs = fileName |> ModuleName.forBsFile;

    let name = EmitTyp.componentExportName(~language, ~fileName, ~moduleName);
    let jsProps = "jsProps";
    let jsPropsDot = s => jsProps ++ "." ++ s;

    let args =
      switch (converter) {
      | FunctionC(groupedArgConverters, _retConverter) =>
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
                 |> Converter.toReason(~converter=argConverter, ~enumTables)
               )
          )
          @ [
            jsPropsDot("children")
            |> Converter.toReason(~converter=childrenConverter, ~enumTables),
          ]

        | [ArgConverter(_, childrenConverter), ..._] => [
            jsPropsDot("children")
            |> Converter.toReason(~converter=childrenConverter, ~enumTables),
          ]

        | _ => [jsPropsDot("children")]
        }

      | _ => [jsPropsDot("children")]
      };

    let emitters =
      emitExportType(~emitters, ~language, ~typIsOpaque, exportType);

    let numArgs = args |> List.length;
    let useCurry = numArgs >= 2;

    let emitCurry = (~args, name) =>
      switch (numArgs) {
      | 0
      | 1 => name ++ EmitText.parens(args)
      | (2 | 3 | 4 | 5 | 6 | 7 | 8) as n =>
        "Curry._" ++ (n |> string_of_int) ++ EmitText.parens([name] @ args)
      | _ => "Curry.app" ++ EmitText.parens([name, args |> EmitText.array])
      };

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
          ++ (
            ModuleName.toString(moduleNameBs)
            ++ "."
            ++ "make"
            |> emitCurry(~args)
          )
          ++ ";",
          "  }));",
        ],
      );

    let emitters =
      /* only export default for the top level component in the file */
      fileName == moduleName ?
        EmitTyp.emitExportDefault(~emitters, ~config, name) : emitters;

    let env = moduleNameBs |> requireModule(~early=false, ~env, ~importPath);

    let env =
      ModuleName.reasonReact
      |> requireModule(
           ~early=false,
           ~env,
           ~importPath=ImportPath.reasonReactPath(~config),
         );

    let env =
      useCurry ?
        ModuleName.curry
        |> requireModule(
             ~early=false,
             ~env,
             ~importPath=ImportPath.bsCurryPath(~config),
           ) :
        env;

    (env, emitters);

  | ExportValue({fileName, resolvedName, valueAccessPath, typ}) =>
    let importPath =
      fileName
      |> ModuleResolver.resolveModule(
           ~config,
           ~outputFileRelative,
           ~resolver,
           ~importExtension=".bs",
         );
    let fileNameBs = fileName |> ModuleName.forBsFile;
    let envWithRequires =
      fileNameBs |> requireModule(~early=false, ~env, ~importPath);
    let converter = typ |> typToConverter;

    let emitters =
      (
        (fileNameBs |> ModuleName.toString)
        ++ "."
        ++ valueAccessPath
        |> Converter.toJS(~converter, ~enumTables)
      )
      ++ ";"
      |> EmitTyp.emitExportConst(~emitters, ~name=resolvedName, ~typ, ~config);

    (envWithRequires, emitters);
  };
}
and emitCodeItems =
    (
      ~config,
      ~outputFileRelative,
      ~resolver,
      ~emitters,
      ~env,
      ~enumTables,
      ~typIsOpaque,
      ~typToConverter,
      codeItems,
    ) =>
  codeItems
  |> List.fold_left(
       ((env, emitters)) =>
         emitCodeItem(
           ~config,
           ~outputFileRelative,
           ~resolver,
           ~emitters,
           ~env,
           ~enumTables,
           ~typIsOpaque,
           ~typToConverter,
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

let emitEnumTables = (~emitters, enumTables) => {
  let emitTable = (~hash, ~toJS, enum) =>
    "const "
    ++ hash
    ++ " = {"
    ++ (
      enum.cases
      |> List.map(label => {
           let js = label.labelJS |> EmitText.quotes;
           let re = label.label |> Runtime.emitVariantLabel(~comment=false);
           toJS ? (re |> EmitText.quotes) ++ ": " ++ js : js ++ ": " ++ re;
         })
      |> String.concat(", ")
    )
    ++ "};";
  Hashtbl.fold(
    (hash, (enum, toJS), emitters) =>
      enum |> emitTable(~hash, ~toJS) |> Emitters.import(~emitters),
    enumTables,
    emitters,
  );
};

let emitImportType =
    (
      ~config as {language, _} as config,
      ~outputFileRelative,
      ~resolver,
      ~emitters,
      ~inputCmtTranslateTypeDeclarations,
      ~env,
      {Translation.typeName, asTypeName, importPath, cmtFile},
    ) => {
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
        |> inputCmtTranslateTypeDeclarations(
             ~config,
             ~outputFileRelative,
             ~resolver,
           )
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

let emitImportTypes =
    (
      ~config,
      ~outputFileRelative,
      ~resolver,
      ~emitters,
      ~env,
      ~inputCmtTranslateTypeDeclarations,
      importTypes,
    ) =>
  importTypes
  |> List.fold_left(
       ((env, emitters)) =>
         emitImportType(
           ~config,
           ~outputFileRelative,
           ~resolver,
           ~emitters,
           ~inputCmtTranslateTypeDeclarations,
           ~env,
         ),
       (env, emitters),
     );

let inlineAnnotatedTypes =
    (~config, ~typeDeclarations, typeMap: Translation.typeMap) => {
  let visited = ref(StringSet.empty);
  let markedAsGenType = ref(StringSet.empty);
  let initialAnnotatedTypes =
    typeMap
    |> StringMap.bindings
    |> List.filter(((_, (_, _, genTypeKind, _))) => genTypeKind == GenType);
  let inlineTyp = ((_typeName, (_, typ, genTypeKind, _))) => {
    let rec visit = typ =>
      switch (typ) {
      | Ident(typeName, _) =>
        if (visited^ |> StringSet.mem(typeName)) {
          ();
        } else {
          visited := visited^ |> StringSet.add(typeName);
          switch (typeMap |> StringMap.find(typeName)) {
          | (_, _, GenType | GenTypeOpaque | Generated, _) => ()
          | (_, typ1, NoGenType, _) =>
            markedAsGenType := markedAsGenType^ |> StringSet.add(typeName);
            logItem("Marking type %s as GenType\n", typeName);
            typ1 |> visit;
          | exception Not_found => ()
          };
        }
      | Array(t, _) => t |> visit
      | Enum(_) => ()
      | Function({argTypes, retType}) =>
        argTypes |> List.iter(visit);
        retType |> visit;
      | GroupOfLabeledArgs(fields)
      | Object(fields)
      | Record(fields) => fields |> List.iter(({typ}) => typ |> visit)
      | Option(t)
      | Nullable(t) => t |> visit
      | Tuple(innerTypes) => innerTypes |> List.iter(visit)
      | TypeVar(_) => ()
      };
    switch (genTypeKind) {
    | GenType => typ |> visit
    | Generated
    | GenTypeOpaque
    | NoGenType => ()
    };
  };
  if (config.inlineAnnotations) {
    initialAnnotatedTypes |> List.iter(inlineTyp);
  };
  let newTypeMap =
    typeMap
    |> StringMap.mapi((typeName, (args, typ, genTypeKind, importTypes)) =>
         (
           args,
           typ,
           markedAsGenType^ |> StringSet.mem(typeName) ?
             GenType : genTypeKind,
           importTypes,
         )
       );

  let annotatedTypeDeclarations =
    typeDeclarations
    |> List.map(typeDeclaration =>
         switch (
           typeDeclaration.Translation.exportFromTypeDeclaration.exportKind
         ) {
         | ExportType(exportType) =>
           if (markedAsGenType^ |> StringSet.mem(exportType.resolvedTypeName)) {
             {
               ...typeDeclaration,
               exportFromTypeDeclaration: {
                 ...typeDeclaration.exportFromTypeDeclaration,
                 genTypeKind: GenType,
               },
             };
           } else {
             typeDeclaration;
           }
         | _ => typeDeclaration
         }
       )
    |> List.filter(
         (
           {exportFromTypeDeclaration: {genTypeKind}}: Translation.typeDeclaration,
         ) =>
         genTypeKind != NoGenType
       );

  (newTypeMap, annotatedTypeDeclarations);
};

let emitTranslationAsString =
    (
      ~config,
      ~outputFileRelative,
      ~resolver,
      ~inputCmtTranslateTypeDeclarations,
      translation: Translation.t,
    ) => {
  let language = config.language;

  let initialEnv = {
    requires: ModuleNameMap.empty,
    requiresEarly: ModuleNameMap.empty,
    cmtExportTypeMapCache: StringMap.empty,
    typesFromOtherFiles: StringMap.empty,
  };
  let enumTables = Hashtbl.create(1);
  let (exportTypeMap, annotatedTypeDeclarations) =
    translation.typeDeclarations
    |> createExportTypeMap(~language)
    |> inlineAnnotatedTypes(
         ~config,
         ~typeDeclarations=translation.typeDeclarations,
       );

  let importTypesFromTypeDeclarations =
    annotatedTypeDeclarations
    |> List.map((typeDeclaration: Translation.typeDeclaration) =>
         typeDeclaration.importTypes
       )
    |> List.concat;

  let exportFromTypeDeclarations =
    annotatedTypeDeclarations
    |> List.map((typeDeclaration: Translation.typeDeclaration) =>
         typeDeclaration.exportFromTypeDeclaration
       );

  let typIsOpaque_ = (~env, typ) =>
    typ
    |> Converter.typToConverterOpaque(
         ~language,
         ~exportTypeMap,
         ~typesFromOtherFiles=env.typesFromOtherFiles,
       )
    |> snd;

  let typToConverter_ = (~env, typ) =>
    typ
    |> Converter.typToConverter(
         ~language,
         ~exportTypeMap,
         ~typesFromOtherFiles=env.typesFromOtherFiles,
       );

  let emitters = Emitters.initial
  and env = initialEnv;

  let (env, emitters) =
    /* imports from type declarations go first to build up type tables */
    importTypesFromTypeDeclarations
    @ translation.importTypes
    |> List.sort_uniq(Translation.importTypeCompare)
    |> emitImportTypes(
         ~config,
         ~outputFileRelative,
         ~resolver,
         ~emitters,
         ~env,
         ~inputCmtTranslateTypeDeclarations,
       );

  let (env, emitters) =
    exportFromTypeDeclarations
    |> emitExportFromTypeDeclarations(
         ~config,
         ~emitters,
         ~language,
         ~typIsOpaque=typIsOpaque_(~env),
         ~env,
         ~typToConverter=typToConverter_(~env),
         ~enumTables,
       );

  let (finalEnv, emitters) =
    translation.codeItems
    |> emitCodeItems(
         ~config,
         ~outputFileRelative,
         ~resolver,
         ~emitters,
         ~env,
         ~enumTables,
         ~typIsOpaque=typIsOpaque_(~env),
         ~typToConverter=typToConverter_(~env),
       );

  let emitters = enumTables |> emitEnumTables(~emitters);

  emitters
  |> emitRequires(~early=true, ~language, ~requires=finalEnv.requiresEarly)
  |> emitRequires(~early=false, ~language, ~requires=finalEnv.requires)
  |> Emitters.toString(~separator="\n\n");
};