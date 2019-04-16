open GenTypeCommon;

type env = {
  requiresEarly: ModuleNameMap.t((ImportPath.t, bool)),
  requires: ModuleNameMap.t((ImportPath.t, bool)),
  /* For each .cmt we import types from, keep the map of exported types. */
  cmtToExportTypeMap: StringMap.t(CodeItem.exportTypeMap),
  /* Map of types imported from other files. */
  exportTypeMapFromOtherFiles: CodeItem.exportTypeMap,
  importedValueOrComponent: bool,
};

let requireModule = (~import, ~env, ~importPath, ~strict=false, moduleName) => {
  let requires = import ? env.requiresEarly : env.requires;
  let requiresNew =
    requires
    |> ModuleNameMap.add(
         moduleName,
         (
           moduleName |> ModuleResolver.resolveSourceModule(~importPath),
           strict,
         ),
       );
  import ?
    {...env, requiresEarly: requiresNew} : {...env, requires: requiresNew};
};

let createExportTypeMap =
    (~config, ~file, typeDeclarations: list(CodeItem.typeDeclaration))
    : CodeItem.exportTypeMap => {
  if (Debug.codeItems^) {
    logItem("Create Type Map for %s\n", file);
  };
  let updateExportTypeMap =
      (
        exportTypeMap: CodeItem.exportTypeMap,
        typeDeclaration: CodeItem.typeDeclaration,
      )
      : CodeItem.exportTypeMap => {
    let addExportType =
        (
          ~annotation,
          {resolvedTypeName, typeVars, optType, _}: CodeItem.exportType,
        ) => {
      if (Debug.codeItems^) {
        logItem(
          "Type Map: %s%s%s\n",
          resolvedTypeName |> ResolvedName.toString,
          typeVars == [] ?
            "" : "(" ++ (typeVars |> String.concat(",")) ++ ")",
          switch (optType) {
          | Some(type_) =>
            " "
            ++ (annotation |> Annotation.toString |> EmitText.comment)
            ++ " = "
            ++ (
              type_
              |> EmitType.typeToString(~config, ~typeNameIsInterface=_ =>
                   false
                 )
            )
          | None => ""
          },
        );
      };
      switch (optType) {
      | Some(type_) =>
        exportTypeMap
        |> StringMap.add(
             resolvedTypeName |> ResolvedName.toString,
             {CodeItem.typeVars, type_, annotation},
           )
      | None => exportTypeMap
      };
    };
    switch (typeDeclaration.exportFromTypeDeclaration) {
    | {exportType, annotation} => exportType |> addExportType(~annotation)
    };
  };
  typeDeclarations |> List.fold_left(updateExportTypeMap, StringMap.empty);
};

let codeItemToString = (~config, ~typeNameIsInterface, codeItem: CodeItem.t) =>
  switch (codeItem) {
  | ExportComponent({nestedModuleName, _}) =>
    "ExportComponent nestedModuleName:"
    ++ (
      switch (nestedModuleName) {
      | Some(moduleName) => moduleName |> ModuleName.toString
      | None => ""
      }
    )
  | ExportValue({resolvedName, type_, _}) =>
    "ExportValue"
    ++ " resolvedName:"
    ++ resolvedName
    ++ " type:"
    ++ EmitType.typeToString(~config, ~typeNameIsInterface, type_)
  | ImportComponent({importAnnotation, _}) =>
    "ImportComponent " ++ (importAnnotation.importPath |> ImportPath.toString)
  | ImportValue({importAnnotation, _}) =>
    "ImportValue " ++ (importAnnotation.importPath |> ImportPath.toString)
  };

let emitExportType =
    (
      ~early=?,
      ~emitters,
      ~config,
      ~typeGetNormalized,
      ~typeNameIsInterface,
      {CodeItem.nameAs, opaque, optType, typeVars, resolvedTypeName, _},
    ) => {
  let (opaque, optType) =
    switch (opaque, optType) {
    | (Some(opaque), _) => (opaque, optType)
    | (None, Some(type_)) =>
      let normalized = type_ |> typeGetNormalized;
      normalized == None ? (true, optType) : (false, normalized);
    | (None, None) => (false, None)
    };
  resolvedTypeName
  |> ResolvedName.toString
  |> EmitType.emitExportType(
       ~early?,
       ~config,
       ~emitters,
       ~nameAs,
       ~opaque,
       ~optType,
       ~typeNameIsInterface,
       ~typeVars,
     );
};

let typeNameIsInterface =
    (
      ~exportTypeMap: CodeItem.exportTypeMap,
      ~exportTypeMapFromOtherFiles: CodeItem.exportTypeMap,
      typeName,
    ) => {
  let typeIsInterface = type_ =>
    switch (type_) {
    | Object(_)
    | Record(_) => true
    | _ => false
    };
  switch (exportTypeMap |> StringMap.find(typeName)) {
  | {type_, _} => type_ |> typeIsInterface
  | exception Not_found =>
    switch (exportTypeMapFromOtherFiles |> StringMap.find(typeName)) {
    | {type_, _} => type_ |> typeIsInterface
    | exception Not_found => false
    }
  };
};

let emitExportFromTypeDeclaration =
    (
      ~config,
      ~emitters,
      ~typeGetNormalized,
      ~env,
      ~typeNameIsInterface,
      exportFromTypeDeclaration: CodeItem.exportFromTypeDeclaration,
    ) => (
  env,
  exportFromTypeDeclaration.exportType
  |> emitExportType(
       ~emitters,
       ~config,
       ~typeGetNormalized,
       ~typeNameIsInterface,
     ),
);

let emitExportFromTypeDeclarations =
    (
      ~config,
      ~emitters,
      ~env,
      ~typeGetNormalized,
      ~typeNameIsInterface,
      exportFromTypeDeclarations,
    ) =>
  exportFromTypeDeclarations
  |> List.fold_left(
       ((env, emitters)) =>
         emitExportFromTypeDeclaration(
           ~config,
           ~emitters,
           ~env,
           ~typeGetNormalized,
           ~typeNameIsInterface,
         ),
       (env, emitters),
     );

let rec emitCodeItem =
        (
          ~config,
          ~emitters,
          ~env,
          ~fileName,
          ~outputFileRelative,
          ~resolver,
          ~typeGetConverter,
          ~typeGetNormalized,
          ~typeNameIsInterface,
          ~variantTables,
          codeItem,
        ) => {
  let language = config.language;
  if (Debug.codeItems^) {
    logItem(
      "Code Item: %s\n",
      codeItem |> codeItemToString(~config, ~typeNameIsInterface),
    );
  };
  let indent = Some("");

  switch (codeItem) {
  | ImportComponent({
      asPath,
      childrenTyp,
      exportType,
      importAnnotation,
      propsFields,
      propsTypeName,
    }) =>
    let importPath = importAnnotation.importPath;
    let name = importAnnotation.name;

    let es6 =
      switch (language, config.module_) {
      | (_, ES6)
      | (TypeScript, _) => true
      | (Flow | Untyped, _) => false
      };

    let (firstNameInPath, restOfPath, lastNameInPath) =
      switch (asPath |> Str.split(Str.regexp("\\."))) {
      | [x, ...y] =>
        let lastNameInPath =
          switch (y |> List.rev) {
          | [last, ..._] => last
          | [] => x
          };
        es6 ?
          (x, ["", ...y] |> String.concat("."), lastNameInPath) :
          (name, ["", x, ...y] |> String.concat("."), lastNameInPath);
      | _ => (name, "", name)
      };

    let componentPath = firstNameInPath ++ restOfPath;

    let nameGen = EmitText.newNameGen();

    let (emitters, env) =
      if (es6) {
        /* emit an import {... as ...} immediately */
        let emitters =
          importPath
          |> EmitType.emitImportValueAsEarly(
               ~config,
               ~emitters,
               ~name=firstNameInPath,
               ~nameAs=firstNameInPath == name ? None : Some(firstNameInPath),
             );
        (emitters, env);
      } else {
        /* add an early require(...)  */
        let env =
          firstNameInPath
          |> ModuleName.fromStringUnsafe
          |> requireModule(~import=true, ~env, ~importPath, ~strict=true);
        (emitters, env);
      };
    let componentNameTypeChecked = lastNameInPath ++ "TypeChecked";

    /* Check the type of the component */
    let emitters = EmitType.emitRequireReact(~early=true, ~emitters, ~config);
    let emitters =
      emitExportType(
        ~early=true,
        ~config,
        ~emitters,
        ~typeGetNormalized,
        ~typeNameIsInterface,
        exportType,
      );
    let emitters =
      config.language == Untyped ?
        emitters :
        "("
        ++ (
          "props"
          |> EmitType.ofType(
               ~config,
               ~typeNameIsInterface,
               ~type_=ident(propsTypeName),
             )
        )
        ++ ") {\n  return <"
        ++ componentPath
        ++ " {...props}/>;\n}"
        |> EmitType.emitExportFunction(
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
        "function "
        ++ EmitText.parens(
             (propsFields |> List.map(({name, _}: field) => name))
             @ ["children"]
             |> List.map(EmitType.ofTypeAny(~config)),
           )
        ++ " { return ReasonReact.wrapJsForReason"
        ++ EmitText.parens([
             componentPath,
             "{"
             ++ (
               propsFields
               |> List.map(({name: propName, optional, type_: propTyp, _}) =>
                    propName
                    ++ ": "
                    ++ (
                      propName
                      |> Converter.toJS(
                           ~config,
                           ~converter=
                             (
                               optional == Mandatory ?
                                 propTyp : Option(propTyp)
                             )
                             |> typeGetConverter,
                           ~indent,
                           ~nameGen,
                           ~variantTables,
                         )
                    )
                  )
               |> String.concat(", ")
             )
             ++ "}",
             "children"
             |> Converter.toJS(
                  ~config,
                  ~converter=childrenTyp |> typeGetConverter,
                  ~indent,
                  ~nameGen,
                  ~variantTables,
                ),
           ])
        ++ "; }"
      )
      ++ ";"
      |> EmitType.emitExportConstEarly(
           ~comment=
             "Export '"
             ++ "make"
             ++ "' early to allow circular import from the '.bs.js' file.",
           ~config,
           ~emitters,
           ~name="make",
           ~type_=mixedOrUnknown(~config),
           ~typeNameIsInterface,
         );
    let env =
      ModuleName.reasonReact
      |> requireModule(
           ~import=true,
           ~env,
           ~importPath=ImportPath.reasonReactPath(~config),
         );
    ({...env, importedValueOrComponent: true}, emitters);

  | ImportValue({asPath, importAnnotation, type_, valueName}) =>
    let nameGen = EmitText.newNameGen();
    let importPath = importAnnotation.importPath;
    let (firstNameInPath, restOfPath) =
      valueName == asPath ?
        (valueName, "") :
        (
          switch (asPath |> Str.split(Str.regexp("\\."))) {
          | [x, ...y] => (x, ["", ...y] |> String.concat("."))
          | _ => (asPath, "")
          }
        );
    let (emitters, importedAsName, env) =
      switch (language, config.module_) {
      | (_, ES6)
      | (TypeScript, _) =>
        /* emit an import {... as ...} immediately */
        let valueNameNotChecked = valueName ++ "NotChecked";
        let emitters =
          importPath
          |> EmitType.emitImportValueAsEarly(
               ~config,
               ~emitters,
               ~name=firstNameInPath,
               ~nameAs=Some(valueNameNotChecked),
             );
        (emitters, valueNameNotChecked, env);
      | (Flow | Untyped, _) =>
        /* add an early require(...)  */
        let importFile = importAnnotation.name;

        let importedAsName = importFile ++ "." ++ firstNameInPath;
        let env =
          importFile
          |> ModuleName.fromStringUnsafe
          |> requireModule(~import=true, ~env, ~importPath, ~strict=true);
        (emitters, importedAsName, env);
      };
    let converter = type_ |> typeGetConverter;
    let valueNameTypeChecked = valueName ++ "TypeChecked";

    let emitters =
      (importedAsName ++ restOfPath)
      ++ ";"
      |> EmitType.emitExportConstEarly(
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
           ~emitters,
           ~name=valueNameTypeChecked,
           ~type_,
           ~typeNameIsInterface,
         );
    let emitters =
      (
        valueNameTypeChecked
        |> Converter.toReason(
             ~config,
             ~converter,
             ~indent,
             ~nameGen,
             ~variantTables,
           )
        |> EmitType.emitTypeCast(~config, ~type_, ~typeNameIsInterface)
      )
      ++ ";"
      |> EmitType.emitExportConstEarly(
           ~comment=
             "Export '"
             ++ valueName
             ++ "' early to allow circular import from the '.bs.js' file.",
           ~config,
           ~emitters,
           ~name=valueName,
           ~type_=mixedOrUnknown(~config),
           ~typeNameIsInterface,
         );
    ({...env, importedValueOrComponent: true}, emitters);

  | ExportComponent({
      componentAccessPath,
      componentType,
      exportType,
      nestedModuleName,
      propsTypeName,
      type_,
      valueAccessPath,
    }) =>
    let nameGen = EmitText.newNameGen();
    let converter = type_ |> typeGetConverter;
    let importPath =
      fileName
      |> ModuleResolver.resolveModule(
           ~config,
           ~outputFileRelative,
           ~resolver,
           ~importExtension=".bs",
         );
    let moduleNameBs = fileName |> ModuleName.forBsFile;
    let moduleName =
      switch (nestedModuleName) {
      | Some(moduleName) => moduleName
      | None => fileName
      };

    let name = EmitType.componentExportName(~config, ~fileName, ~moduleName);
    let jsProps = "jsProps";
    let jsPropsDot = s => jsProps ++ "." ++ s;

    let (propConverters, childrenConverter) =
      switch (converter) {
      | FunctionC({argConverters}) =>
        switch (argConverters) {
        | [
            GroupConverter(propConverters),
            ArgConverter(childrenConverter),
            ..._,
          ] => (
            propConverters,
            childrenConverter,
          )
        | [ArgConverter(childrenConverter), ..._] => ([], childrenConverter)

        | _ => ([], IdentC)
        }

      | _ => ([], IdentC)
      };

    let args =
      (
        propConverters
        |> List.map(((s, optional, argConverter)) =>
             jsPropsDot(s)
             |> Converter.toReason(
                  ~config,
                  ~converter=
                    optional == Optional
                    && !(
                         argConverter
                         |> Converter.converterIsIdentity(~toJS=false)
                       ) ?
                      OptionC(argConverter) : argConverter,
                  ~indent,
                  ~nameGen,
                  ~variantTables,
                )
           )
      )
      @ [
        jsPropsDot("children")
        |> Converter.toReason(
             ~config,
             ~converter=childrenConverter,
             ~indent,
             ~nameGen,
             ~variantTables,
           ),
      ];

    let emitters =
      emitExportType(
        ~emitters,
        ~config,
        ~typeGetNormalized,
        ~typeNameIsInterface,
        exportType,
      );

    let emitters =
      EmitType.emitExportConst(
        ~config,
        ~emitters,
        ~name,
        ~type_=componentType,
        ~typeNameIsInterface,
        [
          "ReasonReact.wrapReasonForJs(",
          "  "
          ++ ModuleName.toString(moduleNameBs)
          ++ "."
          ++ componentAccessPath
          ++ ",",
          "  (function _("
          ++ EmitType.ofType(
               ~config,
               ~typeNameIsInterface,
               ~type_=ident(propsTypeName),
               jsProps,
             )
          ++ ") {",
          "     return "
          ++ (
            ModuleName.toString(moduleNameBs)
            ++ "."
            ++ valueAccessPath
            |> EmitText.curry(~args, ~numArgs=args |> List.length)
          )
          ++ ";",
          "  }));",
        ]
        |> String.concat("\n"),
      );

    let emitters =
      /* only export default for the top level component in the file */
      fileName == moduleName ?
        EmitType.emitExportDefault(~emitters, ~config, name) : emitters;

    let env = moduleNameBs |> requireModule(~import=false, ~env, ~importPath);

    let env =
      ModuleName.reasonReact
      |> requireModule(
           ~import=false,
           ~env,
           ~importPath=ImportPath.reasonReactPath(~config),
         );

    let numArgs = args |> List.length;
    let useCurry = numArgs >= 2;
    config.emitImportCurry = config.emitImportCurry || useCurry;
    (env, emitters);

  | ExportValue({resolvedName, type_, valueAccessPath}) =>
    let nameGen = EmitText.newNameGen();
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
      fileNameBs |> requireModule(~import=false, ~env, ~importPath);
    let converter = type_ |> typeGetConverter;

    let nameIsDefault = resolvedName == "default";

    let emitters =
      (
        (fileNameBs |> ModuleName.toString)
        ++ "."
        ++ valueAccessPath
        |> Converter.toJS(
             ~config,
             ~converter,
             ~indent,
             ~nameGen,
             ~variantTables,
           )
      )
      ++ ";"
      |> EmitType.emitExportConst(
           ~config,
           ~emitters,
           ~name=nameIsDefault ? Runtime.default : resolvedName,
           ~type_,
           ~typeNameIsInterface,
         );

    let emitters =
      nameIsDefault ?
        EmitType.emitExportDefault(~emitters, ~config, Runtime.default) :
        emitters;

    (envWithRequires, emitters);
  };
}
and emitCodeItems =
    (
      ~config,
      ~outputFileRelative,
      ~emitters,
      ~env,
      ~fileName,
      ~resolver,
      ~typeNameIsInterface,
      ~typeGetConverter,
      ~typeGetNormalized,
      ~variantTables,
      codeItems,
    ) =>
  codeItems
  |> List.fold_left(
       ((env, emitters)) =>
         emitCodeItem(
           ~config,
           ~emitters,
           ~env,
           ~fileName,
           ~outputFileRelative,
           ~resolver,
           ~typeGetConverter,
           ~typeGetNormalized,
           ~typeNameIsInterface,
           ~variantTables,
         ),
       (env, emitters),
     );

let emitRequires =
    (~importedValueOrComponent, ~early, ~config, ~requires, emitters) =>
  ModuleNameMap.fold(
    (moduleName, (importPath, strict), emitters) =>
      importPath
      |> EmitType.emitRequire(
           ~importedValueOrComponent,
           ~early,
           ~emitters,
           ~config,
           ~moduleName,
           ~strict,
         ),
    requires,
    emitters,
  );

let emitVariantTables = (~emitters, variantTables) => {
  let emitTable = (~table, ~toJS, variantC: Converter.variantC) =>
    "const "
    ++ table
    ++ " = {"
    ++ (
      variantC.noPayloads
      |> List.map(case => {
           let js = case.labelJS |> labelJSToString(~alwaysQuotes=!toJS);
           let re =
             case.label
             |> Runtime.emitVariantLabel(
                  ~comment=false,
                  ~polymorphic=variantC.polymorphic,
                );
           toJS ? (re |> EmitText.quotes) ++ ": " ++ js : js ++ ": " ++ re;
         })
      |> String.concat(", ")
    )
    ++ "};";
  Hashtbl.fold(
    ((_, toJS), variantC, l) => [(variantC, toJS), ...l],
    variantTables,
    [],
  )
  |> List.sort(((variantC1, toJS1), (variantC2, toJS2)) => {
       let n = compare(variantC1.Converter.hash, variantC2.hash);
       n != 0 ? n : compare(toJS2, toJS1);
     })
  |> List.fold_left(
       (emitters, (variantC, toJS)) =>
         variantC
         |> emitTable(
              ~table=variantC.Converter.hash |> variantTable(~toJS),
              ~toJS,
            )
         |> Emitters.requireEarly(~emitters),
       emitters,
     );
};

let typeGetInlined = (~config, ~exportTypeMap, type_) =>
  switch (
    type_
    |> Converter.typeGetNormalized(
         ~config,
         ~inline=true,
         ~lookupId=s => exportTypeMap |> StringMap.find(s),
         ~typeNameIsInterface=_ => false,
       )
  ) {
  | None => type_
  | Some(type1) => type1
  };

/* Read the cmt file referenced in an import type,
   and recursively for the import types obtained from reading the cmt file. */
let rec readCmtFilesRecursively =
        (
          ~config,
          ~env,
          ~inputCmtTranslateTypeDeclarations,
          ~outputFileRelative,
          ~resolver,
          {CodeItem.typeName, asTypeName, importPath},
        ) => {
  let updateTypeMapFromOtherFiles = (~asType, ~exportTypeMapFromCmt, env) =>
    switch (exportTypeMapFromCmt |> StringMap.find(typeName)) {
    | (exportTypeItem: CodeItem.exportTypeItem) =>
      let type_ =
        exportTypeItem.type_
        |> typeGetInlined(~config, ~exportTypeMap=exportTypeMapFromCmt);
      {
        ...env,
        exportTypeMapFromOtherFiles:
          env.exportTypeMapFromOtherFiles
          |> StringMap.add(asType, {...exportTypeItem, type_}),
      };
    | exception Not_found => env
    };
  let cmtFile =
    importPath
    |> ImportPath.toCmt(~config, ~outputFileRelative)
    |> Paths.getCmtFile;
  switch (asTypeName) {
  | Some(asType) when cmtFile != "" =>
    switch (env.cmtToExportTypeMap |> StringMap.find(cmtFile)) {
    | exportTypeMapFromCmt =>
      env |> updateTypeMapFromOtherFiles(~asType, ~exportTypeMapFromCmt)

    | exception Not_found =>
      /* cmt file not read before: this ensures termination */
      let typeDeclarations =
        Cmt_format.read_cmt(cmtFile)
        |> inputCmtTranslateTypeDeclarations(
             ~config,
             ~outputFileRelative,
             ~resolver,
           )
        |> ((x: CodeItem.translation) => x.typeDeclarations);

      let exportTypeMapFromCmt =
        typeDeclarations
        |> createExportTypeMap(
             ~config,
             ~file=cmtFile |> Filename.basename |> Filename.chop_extension,
           );
      let cmtToExportTypeMap =
        env.cmtToExportTypeMap |> StringMap.add(cmtFile, exportTypeMapFromCmt);
      let env =
        {...env, cmtToExportTypeMap}
        |> updateTypeMapFromOtherFiles(~asType, ~exportTypeMapFromCmt);

      let newImportTypes =
        typeDeclarations
        |> List.map((typeDeclaration: CodeItem.typeDeclaration) =>
             typeDeclaration.importTypes
           )
        |> List.concat;

      newImportTypes
      |> List.fold_left(
           (env, newImportType) =>
             newImportType
             |> readCmtFilesRecursively(
                  ~config,
                  ~env,
                  ~inputCmtTranslateTypeDeclarations,
                  ~outputFileRelative,
                  ~resolver,
                ),
           env,
         );
    }
  | _ => env
  };
};

let emitImportType =
    (
      ~config,
      ~emitters,
      ~env,
      ~inputCmtTranslateTypeDeclarations,
      ~outputFileRelative,
      ~resolver,
      ~typeNameIsInterface,
      {CodeItem.typeName, asTypeName, importPath} as importType,
    ) => {
  let env =
    importType
    |> readCmtFilesRecursively(
         ~config,
         ~env,
         ~inputCmtTranslateTypeDeclarations,
         ~outputFileRelative,
         ~resolver,
       );
  let emitters =
    EmitType.emitImportTypeAs(
      ~emitters,
      ~config,
      ~typeName,
      ~asTypeName,
      ~typeNameIsInterface=typeNameIsInterface(~env),
      ~importPath,
    );
  (env, emitters);
};

let emitImportTypes =
    (
      ~config,
      ~emitters,
      ~env,
      ~inputCmtTranslateTypeDeclarations,
      ~outputFileRelative,
      ~resolver,
      ~typeNameIsInterface,
      importTypes,
    ) =>
  importTypes
  |> List.fold_left(
       ((env, emitters)) =>
         emitImportType(
           ~config,
           ~emitters,
           ~env,
           ~inputCmtTranslateTypeDeclarations,
           ~outputFileRelative,
           ~resolver,
           ~typeNameIsInterface,
         ),
       (env, emitters),
     );

let getAnnotatedTypedDeclarations = (~annotatedSet, typeDeclarations) =>
  typeDeclarations
  |> List.map(typeDeclaration => {
       let nameInAnnotatedSet =
         annotatedSet
         |> StringSet.mem(
              typeDeclaration.CodeItem.exportFromTypeDeclaration.exportType.
                resolvedTypeName
              |> ResolvedName.toString,
            );
       if (nameInAnnotatedSet) {
         {
           ...typeDeclaration,
           exportFromTypeDeclaration: {
             ...typeDeclaration.exportFromTypeDeclaration,
             annotation: GenType,
           },
         };
       } else {
         typeDeclaration;
       };
     })
  |> List.filter(
       (
         {exportFromTypeDeclaration: {annotation, _}, _}: CodeItem.typeDeclaration,
       ) =>
       annotation != NoGenType
     );

let propagateAnnotationToSubTypes =
    (~codeItems, typeMap: CodeItem.exportTypeMap) => {
  let annotatedSet = ref(StringSet.empty);
  let initialAnnotatedTypes =
    typeMap
    |> StringMap.bindings
    |> List.filter(((_, {CodeItem.annotation, _})) =>
         annotation == Annotation.GenType
       )
    |> List.map(((_, {CodeItem.type_, _})) => type_);
  let typesOfExportedValue = (codeItem: CodeItem.t) =>
    switch (codeItem) {
    | ExportComponent({type_, _})
    | ExportValue({type_, _})
    | ImportValue({type_, _}) => [type_]
    | _ => []
    };
  let typesOfExportedValues =
    codeItems |> List.map(typesOfExportedValue) |> List.concat;

  let visitTypAndUpdateMarked = type0 => {
    let visited = ref(StringSet.empty);
    let rec visit = type_ =>
      switch (type_) {
      | Ident({name: typeName}) =>
        if (visited^ |> StringSet.mem(typeName)) {
          ();
        } else {
          visited := visited^ |> StringSet.add(typeName);
          switch (typeMap |> StringMap.find(typeName)) {
          | {annotation: GenType | GenTypeOpaque, _} => ()
          | {type_: type1, annotation: NoGenType, _} =>
            if (Debug.translation^) {
              logItem("Marking Type As Annotated %s\n", typeName);
            };
            annotatedSet := annotatedSet^ |> StringSet.add(typeName);
            type1 |> visit;
          | exception Not_found =>
            annotatedSet := annotatedSet^ |> StringSet.add(typeName)
          };
        }
      | Array(t, _) => t |> visit
      | Function({argTypes, retType, _}) =>
        argTypes |> List.iter(visit);
        retType |> visit;
      | GroupOfLabeledArgs(fields)
      | Object(_, fields)
      | Record(fields) =>
        fields |> List.iter(({type_, _}) => type_ |> visit)
      | Option(t)
      | Null(t)
      | Nullable(t)
      | Promise(t) => t |> visit
      | Tuple(innerTypes) => innerTypes |> List.iter(visit)
      | TypeVar(_) => ()
      | Variant({payloads}) =>
        payloads |> List.iter(((_, _, t)) => t |> visit)
      };
    type0 |> visit;
  };
  initialAnnotatedTypes
  @ typesOfExportedValues
  |> List.iter(visitTypAndUpdateMarked);
  let newTypeMap =
    typeMap
    |> StringMap.mapi((typeName, exportTypeItem: CodeItem.exportTypeItem) =>
         {
           ...exportTypeItem,
           annotation:
             annotatedSet^ |> StringSet.mem(typeName) ?
               Annotation.GenType : exportTypeItem.annotation,
         }
       );

  (newTypeMap, annotatedSet^);
};

let emitTranslationAsString =
    (
      ~config,
      ~fileName,
      ~inputCmtTranslateTypeDeclarations,
      ~outputFileRelative,
      ~resolver,
      translation: Translation.t,
    ) => {
  let initialEnv = {
    requires: ModuleNameMap.empty,
    requiresEarly: ModuleNameMap.empty,
    cmtToExportTypeMap: StringMap.empty,
    exportTypeMapFromOtherFiles: StringMap.empty,
    importedValueOrComponent: false,
  };
  let variantTables = Hashtbl.create(1);

  let (exportTypeMap, annotatedSet) =
    translation.typeDeclarations
    |> createExportTypeMap(~config, ~file=fileName |> ModuleName.toString)
    |> propagateAnnotationToSubTypes(~codeItems=translation.codeItems);

  let annotatedTypeDeclarations =
    translation.typeDeclarations
    |> getAnnotatedTypedDeclarations(~annotatedSet);

  let importTypesFromTypeDeclarations =
    annotatedTypeDeclarations
    |> List.map((typeDeclaration: CodeItem.typeDeclaration) =>
         typeDeclaration.importTypes
       )
    |> List.concat;

  let exportFromTypeDeclarations =
    annotatedTypeDeclarations
    |> List.map((typeDeclaration: CodeItem.typeDeclaration) =>
         typeDeclaration.exportFromTypeDeclaration
       );

  let typeNameIsInterface = (~env) =>
    typeNameIsInterface(
      ~exportTypeMap,
      ~exportTypeMapFromOtherFiles=env.exportTypeMapFromOtherFiles,
    );

  let lookupId_ = (~env, s) =>
    try (exportTypeMap |> StringMap.find(s)) {
    | Not_found => env.exportTypeMapFromOtherFiles |> StringMap.find(s)
    };

  let typeGetNormalized_ = (~env, type_) =>
    type_
    |> Converter.typeGetNormalized(
         ~config,
         ~inline=false,
         ~lookupId=lookupId_(~env),
         ~typeNameIsInterface=typeNameIsInterface(~env),
       );

  let typeGetConverter_ = (~env, type_) =>
    type_
    |> Converter.typeGetConverter(
         ~config,
         ~lookupId=lookupId_(~env),
         ~typeNameIsInterface=typeNameIsInterface(~env),
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
         ~emitters,
         ~env,
         ~inputCmtTranslateTypeDeclarations,
         ~outputFileRelative,
         ~resolver,
         ~typeNameIsInterface,
       );

  let (env, emitters) =
    exportFromTypeDeclarations
    |> emitExportFromTypeDeclarations(
         ~config,
         ~emitters,
         ~typeGetNormalized=typeGetNormalized_(~env),
         ~env,
         ~typeNameIsInterface=typeNameIsInterface(~env),
       );

  let (env, emitters) =
    translation.codeItems
    |> emitCodeItems(
         ~config,
         ~emitters,
         ~env,
         ~fileName,
         ~outputFileRelative,
         ~resolver,
         ~typeGetNormalized=typeGetNormalized_(~env),
         ~typeGetConverter=typeGetConverter_(~env),
         ~typeNameIsInterface=typeNameIsInterface(~env),
         ~variantTables,
       );
  let env =
    config.emitImportCurry ?
      ModuleName.curry
      |> requireModule(
           ~import=true,
           ~env,
           ~importPath=ImportPath.bsCurryPath(~config),
         ) :
      env;

  let finalEnv =
    config.emitCreateBucklescriptBlock ?
      ModuleName.createBucklescriptBlock
      |> requireModule(
           ~import=true,
           ~env,
           ~importPath=ImportPath.bsBlockPath(~config),
         ) :
      env;

  let emitters = variantTables |> emitVariantTables(~emitters);

  emitters
  |> emitRequires(
       ~importedValueOrComponent=false,
       ~early=true,
       ~config,
       ~requires=finalEnv.requiresEarly,
     )
  |> emitRequires(
       ~importedValueOrComponent=finalEnv.importedValueOrComponent,
       ~early=false,
       ~config,
       ~requires=finalEnv.requires,
     )
  |> Emitters.toString(~separator="\n\n");
};