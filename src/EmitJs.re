open GenTypeCommon;

type typeMap = StringMap.t((list(string), typ));

type env = {
  requires: ModuleNameMap.t(ImportPath.t),
  externalReactClass: list(CodeItem.externalReactClass),
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
    | ExternalReactClass(_) => exportTypeMap
    };
  };
  codeItems |> List.fold_left(updateExportTypeMap, StringMap.empty);
};

type emitters = {
  requireEmitter: Emitter.t,
  importEmitter: Emitter.t,
  exportEmitter: Emitter.t,
};

let emitRequire = (~emitters, s) => {
  ...emitters,
  requireEmitter: s |> Emitter.string(~emitter=emitters.requireEmitter),
};
let emitImport = (~emitters, s) => {
  ...emitters,
  importEmitter: s |> Emitter.string(~emitter=emitters.importEmitter),
};

let emitExport = (~emitters, s) => {
  ...emitters,
  exportEmitter: s |> Emitter.string(~emitter=emitters.exportEmitter),
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

  let emitImportType = (~language, ~emitters, ~env, importType) =>
    switch (importType) {
    | CodeItem.ImportComment(s) => (env, s |> emitImport(~emitters))
    | ImportTypeAs({typeName, asTypeName, importPath, cmtFile}) =>
      let emitters =
        EmitTyp.emitImportTypeAs(
          ~language,
          ~typeName,
          ~asTypeName,
          ~importPath,
        )
        |> emitImport(~emitters);

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
        ~language,
        ~emitters,
        {CodeItem.opaque, typeVars, typeName, comment, typ},
      ) => {
    ...emitters,
    exportEmitter:
      typ
      |> EmitTyp.emitExportType(
           ~language,
           ~opaque,
           ~typeName,
           ~typeVars,
           ~comment,
         )
      |> Emitter.string(~emitter=emitters.exportEmitter),
  };

  let emitCheckJsWrapperType = (~env, ~propsTypeName) =>
    switch (env.externalReactClass) {
    | [] => ""

    | [{componentName, _}] =>
      let s =
        "("
        ++ (
          "props" |> EmitTyp.ofType(~language, ~typ=Ident(propsTypeName, []))
        )
        ++ ") {\n      return <"
        ++ componentName
        ++ " {...props}/>;\n    }";
      EmitTyp.emitExportFunction(~name="checkJsWrapperType", ~config, s);

    | [_, ..._] => "// genType warning: found more than one external component annotated with @genType"
    };

  let emitCodeItem = (~exportTypeMap, ~emitters, env, codeItem) => {
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
      emitImportType(~language, ~emitters, ~env, importType)

    | ExportType(exportType) => (
        env,
        emitExportType(~language, ~emitters, exportType),
      )

    | ExportVariantType({CodeItem.typeParams, leafTypes, name}) => (
        env,
        EmitTyp.emitExportVariantType(
          ~language,
          ~name,
          ~typeParams,
          ~leafTypes,
        )
        |> emitExport(~emitters),
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
        |> EmitTyp.emitExportConst(~name=id |> Ident.name, ~typ, ~config)
        |> emitExport(~emitters);
      ({...env, requires}, emitters);

    | ConstructorBinding(
        exportType,
        constructorType,
        argTypes,
        variantName,
        recordValue,
      ) =>
      let emitters = emitExportType(~language, ~emitters, exportType);

      let recordAsInt = recordValue |> Runtime.emitRecordAsInt(~language);
      let emitters =
        if (argTypes == []) {
          recordAsInt
          ++ ";"
          |> EmitTyp.emitExportConst(
               ~name=variantName,
               ~typ=constructorType,
               ~config,
             )
          |> emitExport(~emitters);
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
               ~name=variantName,
               ~typ=constructorType,
               ~config,
             )
          |> emitExport(~emitters);
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
                   jsPropsDot(s)
                   |> Converter.apply(~converter=argConverter, ~toJS=false)
                 )
            )
            @ [
              jsPropsDot("children")
              |> Converter.apply(~converter=childrenConverter, ~toJS=false),
            ]

          | [ArgConverter(_, childrenConverter), ..._] => [
              jsPropsDot("children")
              |> Converter.apply(~converter=childrenConverter, ~toJS=false),
            ]

          | _ => [jsPropsDot("children")]
          }

        | _ => [jsPropsDot("children")]
        };

      let checkJsWrapperType = emitCheckJsWrapperType(~env, ~propsTypeName);

      if (checkJsWrapperType != "") {
        let exportTypeNoChildren =
          switch (exportType.typ) {
          | GroupOfLabeledArgs(fields) =>
            switch (fields |> List.rev) {
            | [_child, ...propFieldsRev] =>
              let typNoChildren =
                GroupOfLabeledArgs(propFieldsRev |> List.rev);
              {...exportType, typ: typNoChildren};
            | [] => exportType
            }
          | _ => exportType
          };
        let emitters =
          emitExportType(~language, ~emitters, exportTypeNoChildren);
        let emitters = checkJsWrapperType |> emitExport(~emitters);
        (env, emitters);
      } else {
        let emitters = emitExportType(~language, ~emitters, exportType);
        let emitters =
          EmitTyp.emitExportConstMany(
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
          )
          |> emitExport(~emitters);

        let emitters =
          EmitTyp.emitExportDefault(~config, name) |> emitExport(~emitters);

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

    | ExternalReactClass({componentName, importPath} as externalReactClass) =>
      let requires =
        env.requires
        |> ModuleNameMap.add(
             ModuleName.fromStringUnsafe(componentName),
             importPath,
           );
      let newEnv = {
        ...env,
        requires,
        externalReactClass: [externalReactClass, ...env.externalReactClass],
      };
      (newEnv, emitters);
    };
  };

  let initialEnv = {
    requires: ModuleNameMap.empty,
    externalReactClass: [],
    cmtExportTypeMapCache: StringMap.empty,
    typesFromOtherFiles: StringMap.empty,
  };
  let initialEmitters = {
    requireEmitter: Emitter.initial,
    importEmitter: Emitter.initial,
    exportEmitter: Emitter.initial,
  };
  let exportTypeMap = codeItems |> createExportTypeMap(~language);
  let (finalEnv, emitters) =
    codeItems
    |> List.fold_left(
         ((env, emitters)) => emitCodeItem(~exportTypeMap, ~emitters, env),
         (initialEnv, initialEmitters),
       );
  let emitters =
    finalEnv.externalReactClass != [] ?
      EmitTyp.requireReact(~language) |> emitRequire(~emitters) : emitters;
  let emitters =
    ModuleNameMap.fold(
      (moduleName, importPath, emitters) =>
        EmitTyp.emitRequire(~language, moduleName, importPath)
        |> emitRequire(~emitters),
      finalEnv.requires,
      emitters,
    );

  [emitters.requireEmitter, emitters.importEmitter, emitters.exportEmitter]
  |> Emitter.concat
  |> Emitter.toString(~separator="\n\n");
};