open GenTypeCommon;

type importType = {
  typeName: string,
  asTypeName: option(string),
  importPath: ImportPath.t,
  cmtFile: option(string),
};

type t = {
  importTypes: list(importType),
  codeItems: list(CodeItem.t),
};

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
  |> List.map(({importTypes, codeItems}) => (importTypes, codeItems))
  |> List.split
  |> (
    ((importTypes, codeItems)) => {
      importTypes: importTypes |> List.concat,
      codeItems: codeItems |> List.concat,
    }
  );

let createExportType =
    (~opaque, ~typeVars, ~optTyp, ~typeEnv, typeName): CodeItem.t => {
  let resolvedTypeName = typeName |> TypeEnv.addModulePath(~typeEnv);
  ExportType({opaque, typeVars, resolvedTypeName, optTyp});
};

let variantLeafTypeName = (typeName, leafName) =>
  String.capitalize(typeName) ++ String.capitalize(leafName);

let translateConstructorDeclaration =
    (
      ~language,
      ~recordGen,
      ~genTypeKind,
      ~typeEnv,
      variantTypeName,
      constructorDeclaration,
    ) => {
  let constructorArgs = constructorDeclaration.Types.cd_args;
  let leafName = constructorDeclaration.Types.cd_id |> Ident.name;
  let leafNameResolved = leafName |> TypeEnv.addModulePath(~typeEnv);
  let argsTranslation =
    Dependencies.translateTypeExprs(~language, ~typeEnv, constructorArgs);
  let argTypes = argsTranslation |> List.map(({Dependencies.typ, _}) => typ);
  let dependencies =
    argsTranslation
    |> List.map(({Dependencies.dependencies, _}) => dependencies)
    |> List.concat;
  /* A valid Reason identifier that we can point UpperCase JS exports to. */

  let variantTypeNameResolved =
    variantLeafTypeName(variantTypeName, leafName)
    |> TypeEnv.addModulePath(~typeEnv);

  let typeVars = argTypes |> TypeVars.freeOfList;

  let variant = {
    name: variantTypeNameResolved,
    params: typeVars |> TypeVars.toTyp,
  };
  let constructorTyp: CodeItem.constructorTyp = {typeVars, argTypes, variant};
  let recordValue =
    recordGen |> Runtime.newRecordValue(~unboxed=constructorArgs == []);
  let codeItems = [
    CodeItem.ExportVariantLeaf({
      exportType: {
        opaque: Some(true),
        typeVars,
        resolvedTypeName: variantTypeNameResolved,
        optTyp: (Some(mixedOrUnknown(~language)), genTypeKind),
      },
      constructorTyp,
      argTypes,
      leafName: leafNameResolved,
      recordValue,
    }),
  ];
  (variant, (dependencies, codeItems));
};

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
          optTyp: (Some(propsType), NoGenType),
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

let translateValueBinding =
    (
      ~config,
      ~outputFileRelative,
      ~resolver,
      ~moduleItemGen,
      ~fileName,
      ~typeEnv,
      valueBinding,
    )
    : t => {
  let {Typedtree.vb_pat, vb_attributes, vb_expr, _} = valueBinding;
  let moduleItem = moduleItemGen |> Runtime.newModuleItem;
  typeEnv |> TypeEnv.updateModuleItem(~moduleItem);
  let typeExpr = vb_expr.exp_type;
  switch (vb_pat.pat_desc, Annotation.getGenTypeKind(vb_attributes)) {
  | (Tpat_var(id, _), GenType) when Ident.name(id) == "make" =>
    id
    |> Ident.name
    |> translateComponent(
         ~config,
         ~outputFileRelative,
         ~resolver,
         ~fileName,
         ~typeEnv,
         ~typeExpr,
       )
  | (Tpat_var(id, _), GenType) =>
    id
    |> Ident.name
    |> translateValue(
         ~config,
         ~outputFileRelative,
         ~resolver,
         ~fileName,
         ~typeEnv,
         ~typeExpr,
       )
  | _ => {importTypes: [], codeItems: []}
  };
};

let translateSignatureValue =
    (
      ~config,
      ~outputFileRelative,
      ~resolver,
      ~fileName,
      ~typeEnv,
      valueDescription: Typedtree.value_description,
    )
    : t => {
  let {Typedtree.val_id, val_desc, val_attributes, _} = valueDescription;
  let typeExpr = val_desc.ctyp_type;
  switch (val_id, Annotation.getGenTypeKind(val_attributes)) {
  | (id, GenType) when Ident.name(id) == "make" =>
    id
    |> Ident.name
    |> translateComponent(
         ~config,
         ~outputFileRelative,
         ~resolver,
         ~fileName,
         ~typeEnv,
         ~typeExpr,
       )
  | (id, GenType) =>
    id
    |> Ident.name
    |> translateValue(
         ~config,
         ~outputFileRelative,
         ~resolver,
         ~fileName,
         ~typeEnv,
         ~typeExpr,
       )
  | _ => {importTypes: [], codeItems: []}
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
          optTyp: (Some(propsTyp), NoGenType),
        },
        importAnnotation:
          importString |> Annotation.importAnnotationFromString,
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
    };

  | (_, _, Some(StringPayload(importString))) => {
      importTypes:
        typeExprTranslation.dependencies
        |> translateDependencies(~config, ~outputFileRelative, ~resolver),

      codeItems: [
        ImportValue({
          valueName,
          importAnnotation:
            importString |> Annotation.importAnnotationFromString,
          typ: typeExprTranslation.typ,
          fileName,
        }),
      ],
    }

  | _ => {importTypes: [], codeItems: []}
  };
};

let hasSomeGADTLeaf = constructorDeclarations =>
  List.exists(
    declaration => declaration.Types.cd_res !== None,
    constructorDeclarations,
  );

type declarationKind =
  | RecordDelaration(list(Types.label_declaration))
  | GeneralDeclaration(option(Typedtree.core_type))
  | VariantDeclaration(list(Types.constructor_declaration))
  | ImportTypeDeclaration(string, option(Annotation.attributePayload))
  | NoDeclaration;

let traslateDeclarationKind =
    (
      ~config as {language} as config,
      ~outputFileRelative,
      ~resolver,
      ~typeEnv,
      ~genTypeKind,
      ~typeName,
      ~typeVars,
      ~typeParams,
      declarationKind,
    ) =>
  switch (declarationKind) {
  | GeneralDeclaration(optCoreType) =>
    switch (optCoreType) {
    | None => {
        importTypes: [],
        codeItems: [
          typeName
          |> createExportType(
               ~opaque=Some(true),
               ~typeVars,
               ~optTyp=(Some(mixedOrUnknown(~language)), genTypeKind),
               ~typeEnv,
             ),
        ],
      }
    | Some(coreType) =>
      let typeExprTranslation =
        coreType.Typedtree.ctyp_type
        |> Dependencies.translateTypeExpr(~language, ~typeEnv);
      let opaque = genTypeKind == GenTypeOpaque ? Some(true) : None /* None means don't know */;
      let typ =
        switch (optCoreType, typeExprTranslation.typ) {
        | (Some({ctyp_desc: Ttyp_variant(rowFields, _, _), _}), Enum(enum))
            when rowFields |> List.length == (enum.cases |> List.length) =>
          let cases =
            List.combine(rowFields, enum.cases)
            |> List.map(((field, case)) =>
                 switch (field) {
                 | Typedtree.Ttag(label, attributes, _, _) =>
                   switch (
                     attributes
                     |> Annotation.getAttributePayload(
                          Annotation.tagIsGenTypeAs,
                        )
                   ) {
                   | Some(StringPayload(asLabel)) => {
                       label,
                       labelJS: asLabel,
                     }
                   | _ => {label, labelJS: label}
                   }
                 | Tinherit(_) => case
                 }
               );
          cases |> createEnum;
        | _ => typeExprTranslation.typ
        };
      let codeItems = [
        typeName
        |> createExportType(
             ~opaque,
             ~typeVars,
             ~optTyp=(Some(typ), genTypeKind),
             ~typeEnv,
           ),
      ];
      {
        importTypes:
          typeExprTranslation.dependencies
          |> translateDependencies(~config, ~outputFileRelative, ~resolver),
        codeItems,
      };
    }

  | RecordDelaration(labelDeclarations) =>
    let fieldTranslations =
      labelDeclarations
      |> List.map(({Types.ld_id, ld_mutable, ld_type, ld_attributes, _}) => {
           let name =
             switch (
               ld_attributes
               |> Annotation.getAttributePayload(Annotation.tagIsGenTypeAs)
             ) {
             | Some(StringPayload(s)) => s
             | _ => ld_id |> Ident.name
             };
           let mutability = ld_mutable == Mutable ? Mutable : Immutable;
           (
             name,
             mutability,
             ld_type |> Dependencies.translateTypeExpr(~language, ~typeEnv),
           );
         });
    let dependencies =
      fieldTranslations
      |> List.map(((_, _, {Dependencies.dependencies, _})) => dependencies)
      |> List.concat;
    let fields =
      fieldTranslations
      |> List.map(((name, mutable_, {Dependencies.typ, _})) => {
           let (optional, typ1) =
             switch (typ) {
             | Option(typ1) => (Optional, typ1)
             | _ => (Mandatory, typ)
             };
           {name, optional, mutable_, typ: typ1};
         });
    let optTyp = (Some(Record(fields)), genTypeKind);
    let typeVars = TypeVars.extract(typeParams);
    let opaque = Some(genTypeKind == GenTypeOpaque);
    {
      importTypes:
        dependencies
        |> translateDependencies(~config, ~outputFileRelative, ~resolver),
      codeItems: [
        typeName |> createExportType(~opaque, ~typeVars, ~optTyp, ~typeEnv),
      ],
    };

  | VariantDeclaration(constructorDeclarations) =>
    let leafTypesDepsAndVariantLeafBindings = {
      let recordGen = Runtime.recordGen();
      constructorDeclarations
      |> List.map(constructorDeclaration =>
           translateConstructorDeclaration(
             ~language,
             ~recordGen,
             ~genTypeKind,
             ~typeEnv,
             typeName,
             constructorDeclaration,
           )
         );
    };
    let (variants, depsAndVariantLeafBindings) =
      leafTypesDepsAndVariantLeafBindings |> List.split;
    let (listListDeps, listListItems) =
      depsAndVariantLeafBindings |> List.split;
    let dependencies = listListDeps |> List.concat;
    let items = listListItems |> List.concat;
    let typeParams = TypeVars.(typeParams |> extract |> toTyp);
    let variantTypeNameResolved = typeName |> TypeEnv.addModulePath(~typeEnv);
    let unionType =
      CodeItem.ExportVariantType({
        typeParams,
        variants,
        name: variantTypeNameResolved,
      });
    {
      importTypes:
        dependencies
        |> translateDependencies(~config, ~outputFileRelative, ~resolver),
      codeItems: List.append(items, [unionType]),
    };

  | ImportTypeDeclaration(importString, genTypeAsPayload) =>
    let typeName_ = typeName;
    let nameWithModulePath = typeName_ |> TypeEnv.addModulePath(~typeEnv);
    let (typeName, asTypeName) =
      switch (genTypeAsPayload) {
      | Some(StringPayload(asString)) => (
          asString,
          Some(nameWithModulePath),
        )
      | _ => (nameWithModulePath, None)
      };
    let importTypes = [
      {
        typeName,
        asTypeName,
        importPath: importString |> ImportPath.fromStringUnsafe,
        cmtFile: None,
      },
    ];
    let codeItems = [
      /* Make the imported type usable from other modules by exporting it too. */
      typeName_
      |> createExportType(
           ~opaque=Some(false),
           ~typeVars=[],
           ~optTyp=(None, genTypeKind),
           ~typeEnv,
         ),
    ];
    {
      importTypes,

      codeItems,
    };

  | NoDeclaration => {importTypes: [], codeItems: []}
  };

let translateTypeDeclaration =
    (
      ~config,
      ~outputFileRelative,
      ~resolver,
      ~typeEnv,
      ~genTypeKind: genTypeKind,
      dec: Typedtree.type_declaration,
    )
    : t => {
  typeEnv |> TypeEnv.newType(~name=dec.typ_id |> Ident.name);
  let typeName = Ident.name(dec.typ_id);
  let typeParams = dec.typ_type.type_params;
  let typeVars = TypeVars.extract(typeParams);

  let declarationKind =
    switch (dec.typ_type.type_kind, genTypeKind) {
    | (Type_record(labelDeclarations, _), GenType | GenTypeOpaque) =>
      RecordDelaration(labelDeclarations)

    | (Type_variant(constructorDeclarations), GenType)
        when !hasSomeGADTLeaf(constructorDeclarations) =>
      VariantDeclaration(constructorDeclarations)

    | (Type_abstract, NoGenType) when typeParams == [] =>
      switch (
        dec.typ_attributes
        |> Annotation.getAttributePayload(Annotation.tagIsGenTypeImport)
      ) {
      | Some(StringPayload(importString)) =>
        ImportTypeDeclaration(
          importString,
          dec.typ_attributes
          |> Annotation.getAttributePayload(Annotation.tagIsGenTypeAs),
        )

      | _ => NoDeclaration
      }

    | (Type_abstract, GenType | GenTypeOpaque) =>
      GeneralDeclaration(dec.typ_manifest)

    | _ => NoDeclaration
    };

  declarationKind
  |> traslateDeclarationKind(
       ~config,
       ~outputFileRelative,
       ~resolver,
       ~typeEnv,
       ~genTypeKind,
       ~typeName,
       ~typeVars,
       ~typeParams,
     );
};

let translateTypeDeclarations =
    (
      ~config,
      ~outputFileRelative,
      ~resolver,
      ~typeEnv,
      typeDeclarations: list(Typedtree.type_declaration),
    ) => {
  let genTypeKind =
    switch (typeDeclarations) {
    | [dec, ..._] => dec.typ_attributes |> Annotation.getGenTypeKind
    | [] => NoGenType
    };
  typeDeclarations
  |> List.map(
       translateTypeDeclaration(
         ~config,
         ~outputFileRelative,
         ~resolver,
         ~typeEnv,
         ~genTypeKind,
       ),
     )
  |> combine;
};

let rec translateStructItem =
        (
          ~config,
          ~outputFileRelative,
          ~resolver,
          ~moduleItemGen,
          ~fileName,
          ~typeEnv,
          structItem,
        )
        : t =>
  switch (structItem) {
  | {Typedtree.str_desc: Typedtree.Tstr_type(typeDeclarations), _} =>
    typeDeclarations
    |> translateTypeDeclarations(
         ~config,
         ~outputFileRelative,
         ~resolver,
         ~typeEnv,
       )

  | {Typedtree.str_desc: Tstr_value(_loc, valueBindings), _} =>
    valueBindings
    |> List.map(
         translateValueBinding(
           ~config,
           ~outputFileRelative,
           ~resolver,
           ~moduleItemGen,
           ~fileName,
           ~typeEnv,
         ),
       )
    |> combine

  | {Typedtree.str_desc: Tstr_primitive(valueDescription), _} =>
    /* external declaration */
    valueDescription
    |> translatePrimitive(
         ~config,
         ~outputFileRelative,
         ~resolver,
         ~fileName,
         ~typeEnv,
       )

  | {Typedtree.str_desc: Tstr_module(moduleBinding), _} =>
    moduleBinding
    |> translateModuleBinding(
         ~config,
         ~outputFileRelative,
         ~resolver,
         ~fileName,
         ~typeEnv,
         ~moduleItemGen,
       )
  | {Typedtree.str_desc: Tstr_recmodule(moduleBindings), _} =>
    moduleBindings
    |> List.map(
         translateModuleBinding(
           ~config,
           ~outputFileRelative,
           ~resolver,
           ~fileName,
           ~typeEnv,
           ~moduleItemGen,
         ),
       )
    |> combine

  | _ => {importTypes: [], codeItems: []}
  }
and translateStructure =
    (~config, ~outputFileRelative, ~resolver, ~fileName, ~typeEnv, structure)
    : list(t) => {
  let moduleItemGen = Runtime.moduleItemGen();
  structure.Typedtree.str_items
  |> List.map(structItem =>
       structItem
       |> translateStructItem(
            ~config,
            ~outputFileRelative,
            ~resolver,
            ~moduleItemGen,
            ~fileName,
            ~typeEnv,
          )
     );
}
and translateModuleBinding =
    (
      ~config,
      ~outputFileRelative,
      ~resolver,
      ~fileName,
      ~typeEnv,
      ~moduleItemGen,
      {mb_id, mb_expr, mb_attributes, _}: Typedtree.module_binding,
    )
    : t => {
  let name = mb_id |> Ident.name;
  switch (mb_expr.mod_desc) {
  | Tmod_structure(structure) =>
    let _isAnnotated = mb_attributes |> Annotation.hasGenTypeAnnotation;
    let moduleItem = moduleItemGen |> Runtime.newModuleItem;
    typeEnv |> TypeEnv.updateModuleItem(~moduleItem);
    structure
    |> translateStructure(
         ~config,
         ~outputFileRelative,
         ~resolver,
         ~fileName,
         ~typeEnv=typeEnv |> TypeEnv.newModule(~name),
       )
    |> combine;

  | Tmod_ident(_)
  | Tmod_functor(_)
  | Tmod_apply(_)
  | Tmod_constraint(_)
  | Tmod_unpack(_) => {importTypes: [], codeItems: []}
  };
};

let rec translateModuleDeclaration =
        (
          ~config,
          ~outputFileRelative,
          ~resolver,
          ~fileName,
          ~typeEnv,
          {md_id, md_attributes, md_type, _}: Typedtree.module_declaration,
        ) =>
  switch (md_type.mty_desc) {
  | Tmty_signature(signature) =>
    let name = md_id |> Ident.name;
    let _isAnnotated = md_attributes |> Annotation.hasGenTypeAnnotation;
    signature
    |> translateSignature(
         ~config,
         ~outputFileRelative,
         ~resolver,
         ~fileName,
         ~typeEnv=typeEnv |> TypeEnv.newModule(~name),
       )
    |> combine;
  | Tmty_ident(_)
  | Tmty_functor(_)
  | Tmty_with(_)
  | Tmty_typeof(_)
  | Tmty_alias(_) => {importTypes: [], codeItems: []}
  }
and translateSignatureItem =
    (
      ~config,
      ~outputFileRelative,
      ~resolver,
      ~moduleItemGen,
      ~fileName,
      ~typeEnv,
      signatureItem,
    )
    : t =>
  switch (signatureItem) {
  | {Typedtree.sig_desc: Typedtree.Tsig_type(typeDeclarations), _} =>
    typeDeclarations
    |> translateTypeDeclarations(
         ~config,
         ~outputFileRelative,
         ~resolver,
         ~typeEnv,
       )

  | {Typedtree.sig_desc: Tsig_value(valueDescription), _} =>
    if (valueDescription.val_prim != []) {
      valueDescription
      |> translatePrimitive(
           ~config,
           ~outputFileRelative,
           ~resolver,
           ~fileName,
           ~typeEnv,
         );
    } else {
      let moduleItem = moduleItemGen |> Runtime.newModuleItem;
      typeEnv |> TypeEnv.updateModuleItem(~moduleItem);
      valueDescription
      |> translateSignatureValue(
           ~config,
           ~outputFileRelative,
           ~resolver,
           ~fileName,
           ~typeEnv,
         );
    }

  | {Typedtree.sig_desc: Typedtree.Tsig_module(moduleDeclaration), _} =>
    let moduleItem = moduleItemGen |> Runtime.newModuleItem;
    typeEnv |> TypeEnv.updateModuleItem(~moduleItem);
    moduleDeclaration
    |> translateModuleDeclaration(
         ~config,
         ~outputFileRelative,
         ~resolver,
         ~fileName,
         ~typeEnv,
       );

  | _ => {importTypes: [], codeItems: []}
  }
and translateSignature =
    (~config, ~outputFileRelative, ~resolver, ~fileName, ~typeEnv, signature)
    : list(t) => {
  let moduleItemGen = Runtime.moduleItemGen();
  signature.Typedtree.sig_items
  |> List.map(signatureItem =>
       signatureItem
       |> translateSignatureItem(
            ~config,
            ~outputFileRelative,
            ~resolver,
            ~moduleItemGen,
            ~fileName,
            ~typeEnv,
          )
     );
};