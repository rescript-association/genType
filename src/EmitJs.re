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
    (~config, declarations: list(CodeItem.typeDeclaration))
    : CodeItem.exportTypeMap => {
  let updateExportTypeMap =
      (
        exportTypeMap: CodeItem.exportTypeMap,
        typeDeclaration: CodeItem.typeDeclaration,
      )
      : CodeItem.exportTypeMap => {
    let addExportType =
        (
          ~annotation,
          {resolvedTypeName, typeVars, optTyp, _}: CodeItem.exportType,
        ) => {
      if (Debug.codeItems^) {
        logItem(
          "Type Map: %s%s%s\n",
          resolvedTypeName,
          typeVars == [] ?
            "" : "(" ++ (typeVars |> String.concat(",")) ++ ")",
          switch (optTyp) {
          | Some(typ) =>
            " "
            ++ (annotation |> Annotation.toString |> EmitText.comment)
            ++ " = "
            ++ (
              typ
              |> EmitTyp.typToString(~config, ~typeNameIsInterface=_ => false)
            )
          | None => ""
          },
        );
      };
      switch (optTyp) {
      | Some(typ) =>
        exportTypeMap
        |> StringMap.add(
             resolvedTypeName,
             {CodeItem.typeVars, typ, annotation},
           )
      | None => exportTypeMap
      };
    };
    switch (typeDeclaration.exportFromTypeDeclaration) {
    | {exportType, annotation} => exportType |> addExportType(~annotation)
    };
  };
  declarations |> List.fold_left(updateExportTypeMap, StringMap.empty);
};

let codeItemToString = (~config, ~typeNameIsInterface, codeItem: CodeItem.t) =>
  switch (codeItem) {
  | ExportComponent({fileName, moduleName, _}) =>
    "ExportComponent fileName:"
    ++ (fileName |> ModuleName.toString)
    ++ " moduleName:"
    ++ (moduleName |> ModuleName.toString)
  | ExportValue({fileName, resolvedName, typ, _}) =>
    "ExportValue"
    ++ " resolvedName:"
    ++ resolvedName
    ++ " moduleName:"
    ++ ModuleName.toString(fileName)
    ++ " typ:"
    ++ EmitTyp.typToString(~config, ~typeNameIsInterface, typ)
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
      ~typGetNormalized,
      ~typeNameIsInterface,
      {CodeItem.nameAs, opaque, optTyp, typeVars, resolvedTypeName, _},
    ) => {
  let (opaque, optTyp) =
    switch (opaque, optTyp) {
    | (Some(opaque), _) => (opaque, optTyp)
    | (None, Some(typ)) =>
      let normalized = typ |> typGetNormalized;
      normalized == None ? (true, optTyp) : (false, normalized);
    | (None, None) => (false, None)
    };
  resolvedTypeName
  |> EmitTyp.emitExportType(
       ~early?,
       ~config,
       ~emitters,
       ~nameAs,
       ~opaque,
       ~optTyp,
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
  let typIsInterface = typ =>
    switch (typ) {
    | Object(_)
    | Record(_) => true
    | _ => false
    };
  switch (exportTypeMap |> StringMap.find(typeName)) {
  | {typ, _} => typ |> typIsInterface
  | exception Not_found =>
    switch (exportTypeMapFromOtherFiles |> StringMap.find(typeName)) {
    | {typ, _} => typ |> typIsInterface
    | exception Not_found => false
    }
  };
};

let emitExportFromTypeDeclaration =
    (
      ~config,
      ~emitters,
      ~typGetNormalized,
      ~env,
      ~typeNameIsInterface,
      exportFromTypeDeclaration: CodeItem.exportFromTypeDeclaration,
    ) => (
  env,
  exportFromTypeDeclaration.exportType
  |> emitExportType(
       ~emitters,
       ~config,
       ~typGetNormalized,
       ~typeNameIsInterface,
     ),
);

let emitExportFromTypeDeclarations =
    (
      ~config,
      ~emitters,
      ~typGetNormalized,
      ~env,
      ~typeNameIsInterface,
      exportFromTypeDeclarations,
    ) =>
  exportFromTypeDeclarations
  |> List.fold_left(
       ((env, emitters)) =>
         emitExportFromTypeDeclaration(
           ~config,
           ~emitters,
           ~typGetNormalized,
           ~env,
           ~typeNameIsInterface,
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
          ~typGetNormalized,
          ~typToConverter,
          ~typeNameIsInterface,
          ~useCreateBucklescriptBlock,
          codeItem,
        ) => {
  let language = config.language;
  if (Debug.codeItems^) {
    logItem(
      "Code Item: %s\n",
      codeItem |> codeItemToString(~config, ~typeNameIsInterface),
    );
  };

  switch (codeItem) {
  | ImportComponent({
      asPath,
      childrenTyp,
      exportType,
      fileName,
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
      | _ => (name, es6 ? "" : ".default", name)
      };

    let componentPath = firstNameInPath ++ restOfPath;

    let nameGen = EmitText.newNameGen();

    let (emitters, env) =
      if (es6) {
        /* emit an import {... as ...} immediately */
        let emitters =
          importPath
          |> EmitTyp.emitImportValueAsEarly(
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
    let emitters = EmitTyp.emitRequireReact(~early=true, ~emitters, ~config);
    let emitters =
      emitExportType(
        ~early=true,
        ~config,
        ~emitters,
        ~typGetNormalized,
        ~typeNameIsInterface,
        exportType,
      );
    let emitters =
      config.language == Untyped ?
        emitters :
        "("
        ++ (
          "props"
          |> EmitTyp.ofType(
               ~config,
               ~typeNameIsInterface,
               ~typ=Ident(propsTypeName, []),
             )
        )
        ++ ") {\n  return <"
        ++ componentPath
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
             |> List.map(EmitTyp.ofTypeAnyTS(~config)),
           )
        ++ " { return ReasonReact.wrapJsForReason"
        ++ EmitText.parens([
             componentPath,
             "{"
             ++ (
               propsFields
               |> List.map(({name: propName, optional, typ: propTyp, _}) =>
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
                             |> typToConverter,
                           ~enumTables,
                           ~nameGen,
                           ~useCreateBucklescriptBlock,
                         )
                    )
                  )
               |> String.concat(", ")
             )
             ++ "}",
             "children"
             |> Converter.toJS(
                  ~config,
                  ~converter=childrenTyp |> typToConverter,
                  ~enumTables,
                  ~nameGen,
                  ~useCreateBucklescriptBlock,
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
           ~typeNameIsInterface,
           ~typ=mixedOrUnknown(~config),
           ~config,
         );
    let env =
      ModuleName.reasonReact
      |> requireModule(
           ~import=true,
           ~env,
           ~importPath=ImportPath.reasonReactPath(~config),
         );
    ({...env, importedValueOrComponent: true}, emitters);

  | ImportValue({asPath, fileName, importAnnotation, typ, valueName}) =>
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
          |> EmitTyp.emitImportValueAsEarly(
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
    let converter = typ |> typToConverter;
    let valueNameTypeChecked = valueName ++ "TypeChecked";

    let emitters =
      (importedAsName ++ restOfPath)
      ++ ";"
      |> EmitTyp.emitExportConstEarly(
           ~emitters,
           ~name=valueNameTypeChecked,
           ~typeNameIsInterface,
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
      (
        valueNameTypeChecked
        |> Converter.toReason(
             ~config,
             ~converter,
             ~enumTables,
             ~nameGen,
             ~useCreateBucklescriptBlock,
           )
        |> EmitTyp.emitTypeCast(~config, ~typ, ~typeNameIsInterface)
      )
      ++ ";"
      |> EmitTyp.emitExportConstEarly(
           ~comment=
             "Export '"
             ++ valueName
             ++ "' early to allow circular import from the '.bs.js' file.",
           ~emitters,
           ~name=valueName,
           ~typeNameIsInterface,
           ~typ=mixedOrUnknown(~config),
           ~config,
         );
    ({...env, importedValueOrComponent: true}, emitters);

  | ExportComponent({
      componentAccessPath,
      componentType,
      exportType,
      fileName,
      moduleName,
      propsTypeName,
      typ,
      valueAccessPath,
    }) =>
    let nameGen = EmitText.newNameGen();
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

    let name = EmitTyp.componentExportName(~config, ~fileName, ~moduleName);
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
                 |> Converter.toReason(
                      ~config,
                      ~converter=argConverter,
                      ~enumTables,
                      ~nameGen,
                      ~useCreateBucklescriptBlock,
                    )
               )
          )
          @ [
            jsPropsDot("children")
            |> Converter.toReason(
                 ~config,
                 ~converter=childrenConverter,
                 ~enumTables,
                 ~nameGen,
                 ~useCreateBucklescriptBlock,
               ),
          ]

        | [ArgConverter(_, childrenConverter), ..._] => [
            jsPropsDot("children")
            |> Converter.toReason(
                 ~config,
                 ~converter=childrenConverter,
                 ~enumTables,
                 ~nameGen,
                 ~useCreateBucklescriptBlock,
               ),
          ]

        | _ => [jsPropsDot("children")]
        }

      | _ => [jsPropsDot("children")]
      };

    let emitters =
      emitExportType(
        ~emitters,
        ~config,
        ~typGetNormalized,
        ~typeNameIsInterface,
        exportType,
      );

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
        ~typeNameIsInterface,
        ~typ=componentType,
        ~config,
        [
          "ReasonReact.wrapReasonForJs(",
          "  "
          ++ ModuleName.toString(moduleNameBs)
          ++ "."
          ++ componentAccessPath
          ++ ",",
          "  (function _("
          ++ EmitTyp.ofType(
               ~config,
               ~typeNameIsInterface,
               ~typ=Ident(propsTypeName, []),
               jsProps,
             )
          ++ ") {",
          "     return "
          ++ (
            ModuleName.toString(moduleNameBs)
            ++ "."
            ++ valueAccessPath
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

    let env = moduleNameBs |> requireModule(~import=false, ~env, ~importPath);

    let env =
      ModuleName.reasonReact
      |> requireModule(
           ~import=false,
           ~env,
           ~importPath=ImportPath.reasonReactPath(~config),
         );

    let env =
      useCurry ?
        ModuleName.curry
        |> requireModule(
             ~import=false,
             ~env,
             ~importPath=ImportPath.bsCurryPath(~config),
           ) :
        env;

    (env, emitters);

  | ExportValue({fileName, resolvedName, typ, valueAccessPath}) =>
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
    let converter = typ |> typToConverter;

    let emitters =
      (
        (fileNameBs |> ModuleName.toString)
        ++ "."
        ++ valueAccessPath
        |> Converter.toJS(
             ~config,
             ~converter,
             ~enumTables,
             ~nameGen,
             ~useCreateBucklescriptBlock,
           )
      )
      ++ ";"
      |> EmitTyp.emitExportConst(
           ~emitters,
           ~name=resolvedName,
           ~typeNameIsInterface,
           ~typ,
           ~config,
         );

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
      ~typGetNormalized,
      ~typToConverter,
      ~typeNameIsInterface,
      ~useCreateBucklescriptBlock,
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
           ~typGetNormalized,
           ~typToConverter,
           ~typeNameIsInterface,
           ~useCreateBucklescriptBlock,
         ),
       (env, emitters),
     );

let emitRequires =
    (~importedValueOrComponent, ~early, ~config, ~requires, emitters) =>
  ModuleNameMap.fold(
    (moduleName, (importPath, strict), emitters) =>
      importPath
      |> EmitTyp.emitRequire(
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

let emitEnumTables = (~emitters, enumTables) => {
  let emitTable = (~hash, ~toJS, enumC: Converter.enumC) =>
    "const "
    ++ hash
    ++ " = {"
    ++ (
      enumC.noPayloads
      |> List.map(case => {
           let js = case.labelJS |> labelJSToString(~alwaysQuotes=!toJS);
           let re =
             case.label
             |> Runtime.emitVariantLabel(
                  ~comment=false,
                  ~polymorphic=enumC.polymorphic,
                );
           toJS ? (re |> EmitText.quotes) ++ ": " ++ js : js ++ ": " ++ re;
         })
      |> String.concat(", ")
    )
    ++ "};";
  Hashtbl.fold(
    (hash, (enumC, toJS), emitters) =>
      enumC |> emitTable(~hash, ~toJS) |> Emitters.requireEarly(~emitters),
    enumTables,
    emitters,
  );
};

let emitImportType =
    (
      ~config,
      ~outputFileRelative,
      ~resolver,
      ~emitters,
      ~inputCmtTranslateTypeDeclarations,
      ~typeNameIsInterface,
      ~env,
      {CodeItem.typeName, asTypeName, importPath, cmtFile},
    ) => {
  let (env, emitters) =
    switch (asTypeName, cmtFile) {
    | (None, _)
    | (_, None) => (env, emitters)
    | (Some(asType), Some(cmtFile)) =>
      let updateTypeMapFromOtherFiles = (~exportTypeMapFromCmt) =>
        switch (exportTypeMapFromCmt |> StringMap.find(typeName)) {
        | x => env.exportTypeMapFromOtherFiles |> StringMap.add(asType, x)
        | exception Not_found => exportTypeMapFromCmt
        };
      switch (env.cmtToExportTypeMap |> StringMap.find(cmtFile)) {
      | exportTypeMapFromCmt => (
          {
            ...env,
            exportTypeMapFromOtherFiles:
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
          |> createExportTypeMap(~config);
        let cmtToExportTypeMap =
          env.cmtToExportTypeMap
          |> StringMap.add(cmtFile, exportTypeMapFromCmt);
        (
          {
            ...env,
            cmtToExportTypeMap,
            exportTypeMapFromOtherFiles:
              updateTypeMapFromOtherFiles(~exportTypeMapFromCmt),
          },
          emitters,
        );
      };
    };
  let emitters =
    EmitTyp.emitImportTypeAs(
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
      ~outputFileRelative,
      ~resolver,
      ~emitters,
      ~env,
      ~inputCmtTranslateTypeDeclarations,
      ~typeNameIsInterface,
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
           ~typeNameIsInterface,
           ~env,
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
                resolvedTypeName,
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
    |> List.map(((_, {CodeItem.typ, _})) => typ);
  let typesOfExportedValue = (codeItem: CodeItem.t) =>
    switch (codeItem) {
    | ExportValue({typ, _})
    | ExportComponent({typ, _}) => [typ]
    | _ => []
    };
  let typesOfExportedValues =
    codeItems |> List.map(typesOfExportedValue) |> List.concat;

  let visitTypAndUpdateMarked = typ_ => {
    let visited = ref(StringSet.empty);
    let rec visit = typ =>
      switch (typ) {
      | Ident(typeName, _) =>
        if (visited^ |> StringSet.mem(typeName)) {
          ();
        } else {
          visited := visited^ |> StringSet.add(typeName);
          switch (typeMap |> StringMap.find(typeName)) {
          | {annotation: GenType | GenTypeOpaque | Generated, _} => ()
          | {typ: typ1, annotation: NoGenType, _} =>
            if (Debug.translation^) {
              logItem("Marking Type As Annotated %s\n", typeName);
            };
            annotatedSet := annotatedSet^ |> StringSet.add(typeName);
            typ1 |> visit;
          | exception Not_found =>
            annotatedSet := annotatedSet^ |> StringSet.add(typeName)
          };
        }
      | Array(t, _) => t |> visit
      | Function({argTypes, retType, _}) =>
        argTypes |> List.iter(visit);
        retType |> visit;
      | GroupOfLabeledArgs(fields)
      | Object(fields)
      | Record(fields) => fields |> List.iter(({typ, _}) => typ |> visit)
      | Option(t)
      | Nullable(t) => t |> visit
      | Tuple(innerTypes) => innerTypes |> List.iter(visit)
      | TypeVar(_) => ()
      | Variant({payloads}) =>
        payloads |> List.iter(((_, _, t)) => t |> visit)
      };
    typ_ |> visit;
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
      ~outputFileRelative,
      ~resolver,
      ~inputCmtTranslateTypeDeclarations,
      translation: Translation.t,
    ) => {
  let initialEnv = {
    requires: ModuleNameMap.empty,
    requiresEarly: ModuleNameMap.empty,
    cmtToExportTypeMap: StringMap.empty,
    exportTypeMapFromOtherFiles: StringMap.empty,
    importedValueOrComponent: false,
  };
  let enumTables = Hashtbl.create(1);

  let (exportTypeMap, annotatedSet) =
    translation.typeDeclarations
    |> createExportTypeMap(~config)
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

  let typGetNormalized_ = (~env, typ) =>
    typ
    |> Converter.typToConverterNormalized(
         ~config,
         ~exportTypeMap,
         ~exportTypeMapFromOtherFiles=env.exportTypeMapFromOtherFiles,
         ~typeNameIsInterface=typeNameIsInterface(~env),
       )
    |> snd;

  let typToConverter_ = (~env, typ) =>
    typ
    |> Converter.typToConverter(
         ~config,
         ~exportTypeMap,
         ~exportTypeMapFromOtherFiles=env.exportTypeMapFromOtherFiles,
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
         ~outputFileRelative,
         ~resolver,
         ~emitters,
         ~env,
         ~inputCmtTranslateTypeDeclarations,
         ~typeNameIsInterface,
       );

  let useCreateBucklescriptBlock = ref(false);

  let (env, emitters) =
    exportFromTypeDeclarations
    |> emitExportFromTypeDeclarations(
         ~config,
         ~emitters,
         ~typGetNormalized=typGetNormalized_(~env),
         ~env,
         ~typeNameIsInterface=typeNameIsInterface(~env),
       );

  let (env, emitters) =
    translation.codeItems
    |> emitCodeItems(
         ~config,
         ~outputFileRelative,
         ~resolver,
         ~emitters,
         ~env,
         ~enumTables,
         ~typGetNormalized=typGetNormalized_(~env),
         ~typToConverter=typToConverter_(~env),
         ~typeNameIsInterface=typeNameIsInterface(~env),
         ~useCreateBucklescriptBlock,
       );

  let finalEnv =
    useCreateBucklescriptBlock^ ?
      ModuleName.createBucklescriptBlock
      |> requireModule(
           ~import=false,
           ~env,
           ~importPath=ImportPath.bsBlockPath(~config),
         ) :
      env;

  let emitters = enumTables |> emitEnumTables(~emitters);

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