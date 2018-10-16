open GenTypeCommon;

type t = {
  dependencies: list(Dependencies.path),
  codeItems: list(CodeItem.t),
};

let combine = (translations: list(t)): t =>
  translations
  |> List.map(({dependencies, codeItems}) => (dependencies, codeItems))
  |> List.split
  |> (
    ((dependencies, codeItems)) => {
      dependencies: dependencies |> List.concat,
      codeItems: codeItems |> List.concat,
    }
  );

let exportType =
    (~opaque, ~typeVars, ~resolvedTypeName, ~comment=?, typ)
    : CodeItem.exportType => {
  opaque,
  typeVars,
  resolvedTypeName,
  comment,
  typ,
};

let translateExportType =
    (~opaque, ~typeVars, ~typeName, ~typeEnv, ~comment=?, typ): CodeItem.t => {
  let resolvedTypeName = typeEnv |> TypeEnv.resolveType(~name=typeName);
  ExportType({opaque, typeVars, resolvedTypeName, comment, typ});
};

let variantLeafTypeName = (typeName, leafName) =>
  String.capitalize(typeName) ++ String.capitalize(leafName);

/*
 * TODO: Make the types namespaced by nested Flow module.
 */
let translateConstructorDeclaration =
    (~language, ~recordGen, ~typeEnv, variantTypeName, constructorDeclaration) => {
  let constructorArgs = constructorDeclaration.Types.cd_args;
  let leafName = constructorDeclaration.Types.cd_id |> Ident.name;
  let leafNameResolved = typeEnv |> TypeEnv.resolveType(~name=leafName);
  let argsTranslation =
    Dependencies.translateTypeExprs(~language, ~typeEnv, constructorArgs);
  let argTypes = argsTranslation |> List.map(({Dependencies.typ, _}) => typ);
  let dependencies =
    argsTranslation
    |> List.map(({Dependencies.dependencies, _}) => dependencies)
    |> List.concat;
  /* A valid Reason identifier that we can point UpperCase JS exports to. */

  let variantTypeNameResolved =
    typeEnv
    |> TypeEnv.resolveType(
         ~name=variantLeafTypeName(variantTypeName, leafName),
       );

  let typeVars = argTypes |> TypeVars.freeOfList;

  let variant = {
    name: variantTypeNameResolved,
    params: typeVars |> TypeVars.toTyp,
  };
  let constructorTyp: CodeItem.constructorTyp = {typeVars, argTypes, variant};
  let recordValue =
    recordGen |> Runtime.newRecordValue(~unboxed=constructorArgs == []);
  let codeItems = [
    CodeItem.WrapVariantLeaf({
      exportType:
        exportType(
          ~opaque=true,
          ~typeVars,
          ~resolvedTypeName=variantTypeNameResolved,
          mixedOrUnknown(~language),
        ),
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
  | Ident(_)
  | TypeVar(_)
  | Option(_)
  | Nullable(_)
  | Array(_)
  | GroupOfLabeledArgs(_)
  | Object(_)
  | Record(_) => typ
  | Function({argTypes, retType, _}) =>
    Function({typeVars, argTypes, retType})
  };

let translateValue = (~language, ~moduleName, ~typeEnv, ~typeExpr, name): t => {
  let typeExprTranslation =
    typeExpr |> Dependencies.translateTypeExpr(~language, ~typeEnv);
  let typeVars = typeExprTranslation.typ |> TypeVars.free;
  let typ = typeExprTranslation.typ |> abstractTheTypeParameters(~typeVars);
  let resolvedName = typeEnv |> TypeEnv.resolveType(~name);

  /* Access path for the value in the module.
     I can be the value name if the module is not nested.
     Or TopLevelModule[x][y] if accessing a value in a doubly nested module */
  let valueAccessPath =
    typeEnv |> TypeEnv.getValueAccessPath(~name=resolvedName);

  let codeItems = [
    CodeItem.WrapReasonValue({
      moduleName,
      resolvedName,
      valueAccessPath,
      typ,
    }),
  ];
  {dependencies: typeExprTranslation.dependencies, codeItems};
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
    (~language, ~propsTypeGen, ~moduleName, ~typeEnv, ~typeExpr, name): t => {
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
          ("children", NonMandatory, mixedOrUnknown(~language)),
        ])
      /* Then we had both props and children. */
      | [children, ..._] =>
        switch (propOrChildren) {
        | GroupOfLabeledArgs(fields) =>
          GroupOfLabeledArgs(
            fields @ [("children", NonMandatory, children)],
          )
        | _ => propOrChildren
        }
      };
    let propsTypeName = GenIdent.propsTypeName(~propsTypeGen);
    let componentType = EmitTyp.reactComponentType(~language, ~propsTypeName);

    let codeItems = [
      CodeItem.WrapReasonComponent({
        exportType:
          exportType(
            ~opaque=false,
            ~typeVars,
            ~resolvedTypeName=propsTypeName,
            propsType,
          ),
        moduleName,
        propsTypeName,
        componentType,
        typ,
      }),
    ];
    {dependencies: typeExprTranslation.dependencies, codeItems};

  | _ =>
    /* not a component: treat make as a normal function */
    name |> translateValue(~language, ~moduleName, ~typeEnv, ~typeExpr)
  };
};

let translateValueBinding =
    (
      ~language,
      ~propsTypeGen,
      ~moduleItemGen,
      ~moduleName,
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
         ~language,
         ~propsTypeGen,
         ~moduleName,
         ~typeEnv,
         ~typeExpr,
       )
  | (Tpat_var(id, _), GenType) =>
    id
    |> Ident.name
    |> translateValue(~language, ~moduleName, ~typeEnv, ~typeExpr)
  | _ => {dependencies: [], codeItems: []}
  };
};

let translateSignatureValue =
    (
      ~language,
      ~propsTypeGen,
      ~moduleName,
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
         ~language,
         ~propsTypeGen,
         ~moduleName,
         ~typeEnv,
         ~typeExpr,
       )
  | (id, GenType) =>
    id
    |> Ident.name
    |> translateValue(~language, ~moduleName, ~typeEnv, ~typeExpr)
  | _ => {dependencies: [], codeItems: []}
  };
};

/**
 * [@genType]
 * [@bs.module] external myBanner : ReasonReact.reactClass = "./MyBanner";
 */
let translatePrimitive =
    (
      ~language,
      ~moduleName,
      ~typeEnv,
      ~propsTypeGen,
      valueDescription: Typedtree.value_description,
    )
    : t => {
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
              |> List.map(((s, optionalness, typ) as field) =>
                   switch (typ, optionalness) {
                   | (Option(typ1), NonMandatory) => (s, NonMandatory, typ1)
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
    let propsTypeName = GenIdent.propsTypeName(~propsTypeGen);

    let codeItems = [
      CodeItem.WrapJsComponent({
        exportType:
          exportType(
            ~opaque=false,
            ~typeVars,
            ~resolvedTypeName=propsTypeName,
            propsTyp,
          ),
        importAnnotation:
          importString |> Annotation.importAnnotationFromString,
        childrenTyp,
        propsFields,
        propsTypeName,
        moduleName,
      }),
    ];
    {dependencies: typeExprTranslation.dependencies, codeItems};

  | (_, _, Some(StringPayload(importString))) => {
      dependencies: typeExprTranslation.dependencies,
      codeItems: [
        WrapJsValue({
          valueName,
          importAnnotation:
            importString |> Annotation.importAnnotationFromString,
          typ: typeExprTranslation.typ,
          moduleName,
        }),
      ],
    }

  | _ => {dependencies: [], codeItems: []}
  };
};

let hasSomeGADTLeaf = constructorDeclarations =>
  List.exists(
    declaration => declaration.Types.cd_res !== None,
    constructorDeclarations,
  );

let translateTypeDeclaration =
    (~language, ~typeEnv, dec: Typedtree.type_declaration): t => {
  typeEnv |> TypeEnv.newType(~name=dec.typ_id |> Ident.name);

  switch (
    dec.typ_type.type_params,
    dec.typ_type.type_kind,
    Annotation.getGenTypeKind(dec.typ_attributes),
  ) {
  | (typeParams, Type_record(labelDeclarations, _), GenType | GenTypeOpaque) =>
    let fieldTranslations =
      labelDeclarations
      |> List.map(({Types.ld_id, ld_type, ld_attributes, _}) => {
           let name =
             switch (
               ld_attributes
               |> Annotation.getAttributePayload(Annotation.tagIsGenTypeAs)
             ) {
             | Some(StringPayload(s)) => s
             | _ => ld_id |> Ident.name
             };
           (
             name,
             ld_type |> Dependencies.translateTypeExpr(~language, ~typeEnv),
           );
         });
    let dependencies =
      fieldTranslations
      |> List.map(((_, {Dependencies.dependencies, _})) => dependencies)
      |> List.concat;
    let fields =
      fieldTranslations
      |> List.map(((name, {Dependencies.typ, _})) => {
           let (optionalNess, typ') =
             switch (typ) {
             | Option(typ') => (NonMandatory, typ')
             | _ => (Mandatory, typ)
             };
           (name, optionalNess, typ');
         });
    let typ = Record(fields);
    let typeVars = TypeVars.extract(typeParams);
    let typeName = Ident.name(dec.typ_id);
    {
      dependencies,
      codeItems: [
        translateExportType(
          ~opaque=false,
          ~typeVars,
          ~typeName,
          ~typeEnv,
          typ,
        ),
      ],
    };

  /*
   * This case includes aliasings such as:
   *
   *     type list('t) = List.t('t');
   */
  | (typeParams, Type_abstract, GenType | GenTypeOpaque)
  | (typeParams, Type_variant(_), GenTypeOpaque) =>
    let typeVars = TypeVars.extract(typeParams);
    let typeName = Ident.name(dec.typ_id);
    switch (dec.typ_manifest) {
    | None => {
        dependencies: [],
        codeItems: [
          translateExportType(
            ~opaque=true,
            ~typeVars,
            ~typeName,
            ~typeEnv,
            mixedOrUnknown(~language),
          ),
        ],
      }
    | Some(coreType) =>
      let typeExprTranslation =
        coreType.Typedtree.ctyp_type
        |> Dependencies.translateTypeExpr(~language, ~typeEnv);
      let rec isOpaque = typ =>
        switch (typ) {
        | Ident(_) => !(typ == booleanT || typ == numberT || typ == stringT)
        | TypeVar(_) => true
        | Option(t) => t |> isOpaque
        | Nullable(t) => t |> isOpaque
        | Array(t) => t |> isOpaque
        | GroupOfLabeledArgs(_) => true
        | Object(_) => false
        | Record(_) => false
        | Function(_) => false
        };
      let opaque = typeExprTranslation.typ |> isOpaque;
      let codeItems = [
        translateExportType(
          ~opaque,
          ~typeVars,
          ~typeName,
          ~typeEnv,
          typeExprTranslation.typ,
        ),
      ];
      {dependencies: typeExprTranslation.dependencies, codeItems};
    };

  | (astTypeParams, Type_variant(constructorDeclarations), GenType)
      when !hasSomeGADTLeaf(constructorDeclarations) =>
    let variantTypeName = dec.typ_id |> Ident.name;
    let leafTypesDepsAndVariantLeafBindings = {
      let recordGen = Runtime.recordGen();
      constructorDeclarations
      |> List.map(constructorDeclaration =>
           translateConstructorDeclaration(
             ~language,
             ~recordGen,
             ~typeEnv,
             variantTypeName,
             constructorDeclaration,
           )
         );
    };
    let (variants, depsAndVariantLeafBindings) =
      leafTypesDepsAndVariantLeafBindings |> List.split;
    let (listListDeps, listListItems) =
      depsAndVariantLeafBindings |> List.split;
    let deps = listListDeps |> List.concat;
    let items = listListItems |> List.concat;
    let typeParams = TypeVars.(astTypeParams |> extract |> toTyp);
    let variantTypeNameResolved =
      typeEnv |> TypeEnv.resolveType(~name=variantTypeName);
    let unionType =
      CodeItem.ExportVariantType({
        typeParams,
        variants,
        name: variantTypeNameResolved,
      });
    {dependencies: deps, codeItems: List.append(items, [unionType])};

  | _ => {dependencies: [], codeItems: []}
  };
};

let rec translateStructItem =
        (
          ~config,
          ~propsTypeGen,
          ~moduleItemGen,
          ~moduleName,
          ~typeEnv,
          structItem,
        )
        : t =>
  switch (structItem) {
  | {Typedtree.str_desc: Typedtree.Tstr_type(typeDeclarations), _} =>
    typeDeclarations
    |> List.map(
         translateTypeDeclaration(~language=config.language, ~typeEnv),
       )
    |> combine

  | {Typedtree.str_desc: Tstr_value(_loc, valueBindings), _} =>
    valueBindings
    |> List.map(
         translateValueBinding(
           ~language=config.language,
           ~propsTypeGen,
           ~moduleItemGen,
           ~moduleName,
           ~typeEnv,
         ),
       )
    |> combine

  | {Typedtree.str_desc: Tstr_primitive(valueDescription), _} =>
    /* external declaration */
    valueDescription
    |> translatePrimitive(
         ~language=config.language,
         ~moduleName,
         ~propsTypeGen,
         ~typeEnv,
       )

  | {Typedtree.str_desc: Tstr_module(moduleBinding), _} =>
    moduleBinding
    |> translateModuleBinding(
         ~config,
         ~moduleName,
         ~typeEnv,
         ~moduleItemGen,
         ~propsTypeGen,
       )
  | {Typedtree.str_desc: Tstr_recmodule(moduleBindings), _} =>
    moduleBindings
    |> List.map(
         translateModuleBinding(
           ~config,
           ~moduleName,
           ~typeEnv,
           ~propsTypeGen,
           ~moduleItemGen,
         ),
       )
    |> combine

  | _ => {dependencies: [], codeItems: []}
  }
and translateStructure =
    (~config, ~propsTypeGen, ~moduleName, ~typeEnv, structure): list(t) => {
  let moduleItemGen = Runtime.moduleItemGen();
  structure.Typedtree.str_items
  |> List.map(structItem =>
       structItem
       |> translateStructItem(
            ~config,
            ~propsTypeGen,
            ~moduleItemGen,
            ~moduleName,
            ~typeEnv,
          )
     );
}
and translateModuleBinding =
    (
      ~config,
      ~moduleName,
      ~typeEnv,
      ~propsTypeGen,
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
    let {dependencies, codeItems} =
      structure
      |> translateStructure(
           ~config,
           ~propsTypeGen,
           ~moduleName,
           ~typeEnv=typeEnv |> TypeEnv.newModule(~name),
         )
      |> combine;
    {dependencies, codeItems};

  | Tmod_ident(_)
  | Tmod_functor(_)
  | Tmod_apply(_)
  | Tmod_constraint(_)
  | Tmod_unpack(_) => {dependencies: [], codeItems: []}
  };
};

let rec translateModuleDeclaration =
        (
          ~config,
          ~propsTypeGen,
          ~moduleName,
          ~typeEnv,
          {md_id, md_attributes, md_type, _}: Typedtree.module_declaration,
        ) =>
  switch (md_type.mty_desc) {
  | Tmty_signature(signature) =>
    let name = md_id |> Ident.name;
    let _isAnnotated = md_attributes |> Annotation.hasGenTypeAnnotation;
    let {dependencies, codeItems} =
      signature
      |> translateSignature(
           ~config,
           ~propsTypeGen,
           ~moduleName,
           ~typeEnv=typeEnv |> TypeEnv.newModule(~name),
         )
      |> combine;
    {dependencies, codeItems};
  | Tmty_ident(_)
  | Tmty_functor(_)
  | Tmty_with(_)
  | Tmty_typeof(_)
  | Tmty_alias(_) => {dependencies: [], codeItems: []}
  }
and translateSignatureItem =
    (
      ~config,
      ~propsTypeGen,
      ~moduleItemGen,
      ~moduleName,
      ~typeEnv,
      signatureItem,
    )
    : t =>
  switch (signatureItem) {
  | {Typedtree.sig_desc: Typedtree.Tsig_type(typeDeclarations), _} =>
    typeDeclarations
    |> List.map(
         translateTypeDeclaration(~language=config.language, ~typeEnv),
       )
    |> combine

  | {Typedtree.sig_desc: Tsig_value(valueDescription), _} =>
    if (valueDescription.val_prim != []) {
      valueDescription
      |> translatePrimitive(
           ~language=config.language,
           ~moduleName,
           ~typeEnv,
           ~propsTypeGen,
         );
    } else {
      let moduleItem = moduleItemGen |> Runtime.newModuleItem;
      typeEnv |> TypeEnv.updateModuleItem(~moduleItem);
      valueDescription
      |> translateSignatureValue(
           ~language=config.language,
           ~propsTypeGen,
           ~moduleName,
           ~typeEnv,
         );
    }

  | {Typedtree.sig_desc: Typedtree.Tsig_module(moduleDeclaration), _} =>
    let moduleItem = moduleItemGen |> Runtime.newModuleItem;
    typeEnv |> TypeEnv.updateModuleItem(~moduleItem);
    moduleDeclaration
    |> translateModuleDeclaration(
         ~config,
         ~propsTypeGen,
         ~moduleName,
         ~typeEnv,
       );

  | _ => {dependencies: [], codeItems: []}
  }
and translateSignature =
    (~config, ~propsTypeGen, ~moduleName, ~typeEnv, signature): list(t) => {
  let moduleItemGen = Runtime.moduleItemGen();
  signature.Typedtree.sig_items
  |> List.map(signatureItem =>
       signatureItem
       |> translateSignatureItem(
            ~config,
            ~propsTypeGen,
            ~moduleItemGen,
            ~moduleName,
            ~typeEnv,
          )
     );
};

let pathToImport = (~config, ~outputFileRelative, ~resolver, path) =>
  switch (path) {
  | Dependencies.Pid(name) when name == "list" => [
      CodeItem.ImportTypeAs({
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
      }),
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
    [ImportTypeAs({typeName, asTypeName, importPath, cmtFile})];
  };

let translateDependencies =
    (~config, ~outputFileRelative, ~resolver, dependencies): list(CodeItem.t) =>
  dependencies
  |> List.map(pathToImport(~config, ~outputFileRelative, ~resolver))
  |> List.concat
  |> List.sort_uniq(CodeItem.importTypeCompare)
  |> List.map(importType => CodeItem.ImportType(importType));