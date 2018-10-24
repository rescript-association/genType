open GenTypeCommon;

type importType = {
  typeName: string,
  asTypeName: option(string),
  importPath: ImportPath.t,
  cmtFile: option(string),
};

type typeDeclaration = {
  exportFromTypeDeclaration: CodeItem.exportFromTypeDeclaration,
  importTypes: list(importType),
};

type t = {
  importTypes: list(importType),
  codeItems: list(CodeItem.t),
  typeDeclarations: list(typeDeclaration),
};

type exportTypeMap =
  StringMap.t((list(string), typ, Annotation.t, list(importType)));

let empty = {importTypes: [], codeItems: [], typeDeclarations: []};

let getImportTypeUniqueName = ({typeName, asTypeName, _}: importType) =>
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
  |> List.map(({importTypes, codeItems, typeDeclarations}) =>
       ((importTypes, codeItems), typeDeclarations)
     )
  |> List.split
  |> (((x, y)) => (x |> List.split, y))
  |> (
    (((importTypes, codeItems), typeDeclarations)) => {
      importTypes: importTypes |> List.concat,
      codeItems: codeItems |> List.concat,
      typeDeclarations: typeDeclarations |> List.concat,
    }
  );

/* Applies type parameters to types (for all) */
let abstractTheTypeParameters = (~typeVars, typ) =>
  switch (typ) {
  | Array(_)
  | Enum(_) => typ
  | Function({argTypes, retType, _}) =>
    Function({typeVars, argTypes, retType})
  | GroupOfLabeledArgs(_)
  | Ident(_)
  | Nullable(_)
  | Object(_)
  | Option(_)
  | Record(_)
  | Tuple(_)
  | TypeVar(_) => typ
  };

let pathToImportType = (~config, ~outputFileRelative, ~resolver, path) =>
  switch (path) {
  | Dependencies.Pid(name) when name == "list" => [
      {
        typeName: "list",
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

  | Pdot(Presolved(_), _) => []

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
        |> ImportPath.toCmt(~outputFileRelative)
        |> Paths.getCmtFile;
      cmtFile == "" ? None : Some(cmtFile);
    };
    [{typeName, asTypeName, importPath, cmtFile}];
  };

let translateDependencies =
    (~config, ~outputFileRelative, ~resolver, dependencies): list(importType) =>
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
      name,
    )
    : t => {
  let typeExprTranslation =
    typeExpr
    |> Dependencies.translateTypeExpr(~language=config.language, ~typeEnv);
  let typeVars = typeExprTranslation.typ |> TypeVars.free;
  let typ = typeExprTranslation.typ |> abstractTheTypeParameters(~typeVars);
  let resolvedName = name |> TypeEnv.addModulePath(~typeEnv);

  /* Access path for the value in the module.
     I can be the value name if the module is not nested.
     Or TopLevelModule[x][y] if accessing a value in a doubly nested module */
  let valueAccessPath =
    typeEnv |> TypeEnv.getValueAccessPath(~name=resolvedName);

  let codeItems = [
    CodeItem.ExportValue({fileName, resolvedName, valueAccessPath, typ}),
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
      name,
    )
    : t => {
  let language = config.language;
  let typeExprTranslation =
    typeExpr
    |> Dependencies.translateTypeExpr(
         ~language,
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
           Some(mixedOrUnknown(~language));
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
            name: "children",
            optional: Optional,
            mutable_: Immutable,
            typ: mixedOrUnknown(~language),
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
                name: "children",
                optional: Optional,
                mutable_: Immutable,
                typ: childrenTyp,
              },
            ],
          )
        | _ => propOrChildren
        }
      };
    let propsTypeName = "Props" |> TypeEnv.addModulePath(~typeEnv);
    let componentType = EmitTyp.reactComponentType(~language, ~propsTypeName);
    let moduleName = typeEnv |> TypeEnv.getCurrentModuleName(~fileName);

    let codeItems = [
      CodeItem.ExportComponent({
        exportType: {
          opaque: Some(false),
          typeVars,
          resolvedTypeName: propsTypeName,
          optTyp: Some(propsType),
        },
        fileName,
        moduleName,
        propsTypeName,
        componentType,
        typ,
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
  let language = config.language;
  let valueName = valueDescription.val_id |> Ident.name;
  let typeExprTranslation =
    valueDescription.val_desc.ctyp_type
    |> Dependencies.translateTypeExpr(~language, ~typeEnv);
  let genTypeImportPayload =
    valueDescription.val_attributes
    |> Annotation.getAttributePayload(Annotation.tagIsGenTypeImport);
  switch (
    typeExprTranslation.typ,
    valueDescription.val_prim,
    genTypeImportPayload,
  ) {
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
      Some(StringPayload(importString)),
    )
      when valueName == "make" =>
    let typeExprTranslation =
      valueDescription.val_desc.ctyp_type
      |> Dependencies.translateTypeExpr(
           ~language,
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
             Some(mixedOrUnknown(~language));
           } else {
             None;
           }
         ),
    );

    let (propsFields, childrenTyp) =
      switch (typ) {
      | Function({argTypes: [propOrChildren, ...childrenOrNil], _}) =>
        switch (childrenOrNil) {
        | [] => ([], mixedOrUnknown(~language))
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
          | _ => ([], mixedOrUnknown(~language))
          }
        }
      | _ => ([], mixedOrUnknown(~language))
      };
    let propsTyp = Object(propsFields);
    let propsTypeName = "Props" |> TypeEnv.addModulePath(~typeEnv);

    let codeItems = [
      CodeItem.ImportComponent({
        exportType: {
          opaque: Some(false),
          typeVars,
          resolvedTypeName: propsTypeName,
          optTyp: Some(propsTyp),
        },
        importAnnotation: importString |> Annotation.importFromString,
        childrenTyp,
        propsFields,
        propsTypeName,
        fileName,
      }),
    ];
    {
      importTypes:
        typeExprTranslation.dependencies
        |> translateDependencies(~config, ~outputFileRelative, ~resolver),
      codeItems,
      typeDeclarations: [],
    };

  | (_, _, Some(StringPayload(importString))) => {
      importTypes:
        typeExprTranslation.dependencies
        |> translateDependencies(~config, ~outputFileRelative, ~resolver),

      codeItems: [
        ImportValue({
          valueName,
          importAnnotation: importString |> Annotation.importFromString,
          typ: typeExprTranslation.typ,
          fileName,
        }),
      ],
      typeDeclarations: [],
    }

  | _ => {importTypes: [], codeItems: [], typeDeclarations: []}
  };
};