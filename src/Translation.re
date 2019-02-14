open GenTypeCommon;

type t = CodeItem.translation;

let empty: t = {importTypes: [], codeItems: [], typeDeclarations: []};

let getImportTypeUniqueName =
    ({typeName, asTypeName, _}: CodeItem.importType) =>
  typeName
  ++ (
    switch (asTypeName) {
    | None => ""
    | Some(s) => "_as_" ++ s
    }
  );

let importTypeCompare = (i1, i2) =>
  compare(i1 |> getImportTypeUniqueName, i2 |> getImportTypeUniqueName);

let combine = (translations: list(t)): t =>
  translations
  |> List.map(({CodeItem.importTypes, codeItems, typeDeclarations}) =>
       ((importTypes, codeItems), typeDeclarations)
     )
  |> List.split
  |> (((x, y)) => (x |> List.split, y))
  |> (
    (((importTypes, codeItems), typeDeclarations)) => {
      CodeItem.importTypes: importTypes |> List.concat,
      codeItems: codeItems |> List.concat,
      typeDeclarations: typeDeclarations |> List.concat,
    }
  );

/* Applies type parameters to types (for all) */
let abstractTheTypeParameters = (~typeVars, typ) =>
  switch (typ) {
  | Array(_) => typ
  | Function({argTypes, retType, uncurried, _}) =>
    Function({argTypes, retType, typeVars, uncurried})
  | GroupOfLabeledArgs(_)
  | Ident(_)
  | Nullable(_)
  | Object(_)
  | Option(_)
  | Record(_)
  | Tuple(_)
  | TypeVar(_)
  | Variant(_) => typ
  };

let rec pathIsResolved = (path: Dependencies.path) =>
  switch (path) {
  | Pid(_) => false
  | Presolved(_) => true
  | Pdot(p, _) => p |> pathIsResolved
  };

let pathToImportType =
    (~config, ~outputFileRelative, ~resolver, path: Dependencies.path) =>
  switch (path) {
  | _ when path |> pathIsResolved => []
  | Pid(name) when name == "list" => [
      {
        CodeItem.typeName: "list",
        asTypeName: None,
        importPath:
          ModuleName.reasonPervasives
          |> ModuleResolver.importPathForReasonModuleName(
               ~config,
               ~outputFileRelative,
               ~resolver,
             ),
        cmtFile: None,
      },
    ]
  | Pid(_) => []
  | Presolved(_) => []

  | Pdot(_) =>
    let rec getOuterModuleName = path =>
      switch (path) {
      | Dependencies.Pid(name)
      | Presolved(name) => name |> ModuleName.fromStringUnsafe
      | Pdot(path1, _) => path1 |> getOuterModuleName
      };
    let rec removeOuterModule = path =>
      switch (path) {
      | Dependencies.Pid(_)
      | Dependencies.Presolved(_) => path
      | Pdot(Pid(_), s) => Dependencies.Pid(s)
      | Pdot(path1, s) => Pdot(path1 |> removeOuterModule, s)
      };
    let moduleName = path |> getOuterModuleName;
    let typeName = path |> removeOuterModule |> Dependencies.typePathToName;
    let nameFromPath = path |> Dependencies.typePathToName;
    let asTypeName = nameFromPath == typeName ? None : Some(nameFromPath);
    let importPath =
      moduleName
      |> ModuleResolver.importPathForReasonModuleName(
           ~config,
           ~outputFileRelative,
           ~resolver,
         );
    let cmtFile = {
      let cmtFile =
        importPath
        |> ImportPath.toCmt(~config, ~outputFileRelative)
        |> Paths.getCmtFile;
      cmtFile == "" ? None : Some(cmtFile);
    };
    [{typeName, asTypeName, importPath, cmtFile}];
  };

let translateDependencies =
    (~config, ~outputFileRelative, ~resolver, dependencies)
    : list(CodeItem.importType) =>
  dependencies
  |> List.map(pathToImportType(~config, ~outputFileRelative, ~resolver))
  |> List.concat;

let translateValue =
    (
      ~config,
      ~outputFileRelative,
      ~resolver,
      ~fileName,
      ~typeEnv,
      ~typeExpr,
      ~addAnnotationsToFunction: typ => typ,
      name,
    )
    : t => {
  let typeExprTranslation =
    typeExpr
    |> TranslateTypeExprFromTypes.translateTypeExprFromTypes(
         ~config,
         ~typeEnv,
       );
  let typeVars = typeExprTranslation.typ |> TypeVars.free;
  let typ =
    typeExprTranslation.typ
    |> abstractTheTypeParameters(~typeVars)
    |> addAnnotationsToFunction;
  let resolvedName = name |> TypeEnv.addModulePath(~typeEnv);

  let valueAccessPath =
    typeEnv |> TypeEnv.getValueAccessPath(~name=resolvedName);

  let codeItems = [
    CodeItem.ExportValue({fileName, resolvedName, typ, valueAccessPath}),
  ];
  {
    importTypes:
      typeExprTranslation.dependencies
      |> translateDependencies(~config, ~outputFileRelative, ~resolver),
    codeItems,
    typeDeclarations: [],
  };
};

/*
 * The `make` function is typically of the type:
 *
 *    (~named, ~args=?, 'childrenType) => ReasonReactComponentSpec<
 *      State,
 *      State,
 *      RetainedProps,
 *      RetainedProps,
 *      Action,
 *    >)
 *
 * We take a reference to that function and turn it into a React component of
 * type:
 *
 *
 *     exports.component = (component : React.Component<Props>);
 *
 * Where `Props` is of type:
 *
 *     {named: number, args?: number}
 */
let translateComponent =
    (
      ~config,
      ~outputFileRelative,
      ~resolver,
      ~fileName,
      ~typeEnv,
      ~typeExpr,
      ~addAnnotationsToFunction: typ => typ,
      name,
    )
    : t => {
  let typeExprTranslation_ =
    typeExpr
    |> TranslateTypeExprFromTypes.translateTypeExprFromTypes(
         ~config,
         /* Only get the dependencies for the prop types.
            The return type is a ReasonReact component. */
         ~noFunctionReturnDependencies=true,
         ~typeEnv,
       );
  let typeExprTranslation = {
    ...typeExprTranslation_,
    typ: typeExprTranslation_.typ |> addAnnotationsToFunction,
  };

  let freeTypeVarsSet = typeExprTranslation.typ |> TypeVars.free_;

  /* Replace type variables in props/children with any. */
  let (typeVars, typ) = (
    [],
    typeExprTranslation.typ
    |> TypeVars.substitute(~f=s =>
         if (freeTypeVarsSet |> StringSet.mem(s)) {
           Some(mixedOrUnknown(~config));
         } else {
           None;
         }
       ),
  );
  switch (typ) {
  | Function({
      argTypes: [propOrChildren, ...childrenOrNil],
      retType:
        Ident(
          "ReasonReact_componentSpec" | "React_componentSpec" |
          "ReasonReact_component" |
          "React_component",
          [_state, ..._],
        ),
      _,
    }) =>
    /* Add children?:any to props type */
    let propsType =
      switch (childrenOrNil) {
      /* Then we only extracted a function that accepts children, no props */
      | [] =>
        GroupOfLabeledArgs([
          {
            mutable_: Immutable,
            name: "children",
            optional: Optional,
            typ: mixedOrUnknown(~config),
          },
        ])
      /* Then we had both props and children. */
      | [childrenTyp, ..._] =>
        switch (propOrChildren) {
        | GroupOfLabeledArgs(fields) =>
          GroupOfLabeledArgs(
            fields
            @ [
              {
                mutable_: Immutable,
                name: "children",
                optional: Optional,
                typ: childrenTyp,
              },
            ],
          )
        | _ => propOrChildren
        }
      };
    let propsTypeName = "Props" |> TypeEnv.addModulePath(~typeEnv);
    let componentType = EmitTyp.reactComponentType(~config, ~propsTypeName);
    let moduleName = typeEnv |> TypeEnv.getCurrentModuleName(~fileName);

    let valueAccessPath = typeEnv |> TypeEnv.getValueAccessPath(~name="make");
    let componentAccessPath =
      typeEnv
      |> TypeEnv.getValueAccessPath(~component=true, ~name="component");

    let codeItems = [
      CodeItem.ExportComponent({
        componentAccessPath,
        componentType,
        exportType: {
          nameAs: None,
          opaque: Some(false),
          optTyp: Some(propsType),
          typeVars,
          resolvedTypeName: propsTypeName,
        },
        fileName,
        moduleName,
        propsTypeName,
        typ,
        valueAccessPath,
      }),
    ];
    {
      importTypes:
        typeExprTranslation.dependencies
        |> translateDependencies(~config, ~outputFileRelative, ~resolver),
      codeItems,
      typeDeclarations: [],
    };

  | _ =>
    /* not a component: treat make as a normal function */
    name
    |> translateValue(
         ~config,
         ~outputFileRelative,
         ~resolver,
         ~fileName,
         ~typeEnv,
         ~typeExpr,
         ~addAnnotationsToFunction,
       )
  };
};

/**
 * [@genType]
 * [@bs.module] external myBanner : ReasonReact.reactClass = "./MyBanner";
 */
let translatePrimitive =
    (
      ~config,
      ~outputFileRelative,
      ~resolver,
      ~fileName,
      ~typeEnv,
      valueDescription: Typedtree.value_description,
    )
    : t => {
  if (Debug.translation^) {
    logItem("Translate Primitive\n");
  };
  let valueName = valueDescription.val_id |> Ident.name;
  let typeExprTranslation =
    valueDescription.val_desc
    |> TranslateCoreType.translateCoreType(~config, ~typeEnv);

  let (attributeImport, attributeRenaming) =
    valueDescription.val_attributes |> Annotation.getAttributeImportRenaming;
  switch (typeExprTranslation.typ, valueDescription.val_prim, attributeImport) {
  | (
      Function({
        argTypes: [_, ..._],
        retType:
          Ident(
            "ReasonReact_componentSpec" | "React_componentSpec" |
            "ReasonReact_component" |
            "React_component",
            [_state, ..._],
          ),
        _,
      }),
      _,
      Some(importString),
    )
      when valueName == "make" =>
    let asPath =
      switch (attributeRenaming) {
      | Some(asPath) => asPath
      | None => ""
      };
    let typeExprTranslation =
      valueDescription.val_desc
      |> TranslateCoreType.translateCoreType(
           ~config,
           /* Only get the dependencies for the prop types.
              The return type is a ReasonReact component. */
           ~noFunctionReturnDependencies=true,
           ~typeEnv,
         );

    let freeTypeVarsSet = typeExprTranslation.typ |> TypeVars.free_;

    /* Replace type variables in props/children with any. */
    let (typeVars, typ) = (
      [],
      typeExprTranslation.typ
      |> TypeVars.substitute(~f=s =>
           if (freeTypeVarsSet |> StringSet.mem(s)) {
             Some(mixedOrUnknown(~config));
           } else {
             None;
           }
         ),
    );

    let (propsFields, childrenTyp) =
      switch (typ) {
      | Function({argTypes: [propOrChildren, ...childrenOrNil], _}) =>
        switch (childrenOrNil) {
        | [] => ([], mixedOrUnknown(~config))
        | [children, ..._] =>
          switch (propOrChildren) {
          | GroupOfLabeledArgs(fields) => (
              fields
              |> List.map(({optional, typ, _} as field) =>
                   switch (typ, optional) {
                   | (Option(typ1), Optional) => {
                       ...field,
                       optional: Optional,
                       typ: typ1,
                     }
                   | _ => field
                   }
                 ),
              children,
            )
          | _ => ([], mixedOrUnknown(~config))
          }
        }
      | _ => ([], mixedOrUnknown(~config))
      };
    let propsTyp = Object(Closed, propsFields);
    let propsTypeName = "Props" |> TypeEnv.addModulePath(~typeEnv);

    let codeItems = [
      CodeItem.ImportComponent({
        asPath,
        childrenTyp,
        exportType: {
          nameAs: None,
          opaque: Some(false),
          optTyp: Some(propsTyp),
          typeVars,
          resolvedTypeName: propsTypeName,
        },
        fileName,
        importAnnotation: importString |> Annotation.importFromString,
        propsFields,
        propsTypeName,
      }),
    ];
    {
      importTypes:
        typeExprTranslation.dependencies
        |> translateDependencies(~config, ~outputFileRelative, ~resolver),
      codeItems,
      typeDeclarations: [],
    };

  | (_, _, Some(importString)) =>
    let asPath =
      switch (attributeRenaming) {
      | Some(asPath) => asPath
      | None => valueName
      };
    {
      importTypes:
        typeExprTranslation.dependencies
        |> translateDependencies(~config, ~outputFileRelative, ~resolver),

      codeItems: [
        ImportValue({
          asPath,
          fileName,
          importAnnotation: importString |> Annotation.importFromString,
          typ: typeExprTranslation.typ,
          valueName,
        }),
      ],
      typeDeclarations: [],
    };

  | _ => {importTypes: [], codeItems: [], typeDeclarations: []}
  };
};