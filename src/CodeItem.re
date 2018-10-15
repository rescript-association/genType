open GenTypeCommon;

type exportType = {
  opaque: bool,
  typeVars: list(string),
  typeName: string,
  comment: option(string),
  typ,
};

type exportVariantType = {
  typeParams: list(typ),
  leafTypes: list(typ),
  name: string,
};

type importTypeAs = {
  typeName: string,
  asTypeName: option(string),
  importPath: ImportPath.t,
  cmtFile: option(string),
};

type importType =
  | ImportComment(string)
  | ImportTypeAs(importTypeAs);

type importAnnotation = {
  name: string,
  importPath: ImportPath.t,
};

type wrapJsComponent = {
  exportType,
  importAnnotation,
  childrenTyp: typ,
  propsFields: fields,
  propsTypeName: string,
  moduleName: ModuleName.t,
};

type wrapJsValue = {
  valueName: string,
  importAnnotation,
  typ,
  moduleName: ModuleName.t,
};

type wrapReasonComponent = {
  exportType,
  moduleName: ModuleName.t,
  propsTypeName: string,
  componentType: typ,
  typ,
};

type wrapReasonValue = {
  moduleName: ModuleName.t,
  id: Ident.t,
  typ,
};

type wrapVariant = {
  exportType,
  constructorTyp: typ,
  argTypes: list(typ),
  variantName: string,
  recordValue: Runtime.recordValue,
};

type wrapModule = {
  moduleName: ModuleName.t,
  isAnnotated: bool,
  codeItems: list(t),
}
and t =
  | ExportType(exportType)
  | ExportVariantType(exportVariantType)
  | ImportType(importType)
  | WrapJsComponent(wrapJsComponent)
  | WrapJsValue(wrapJsValue)
  | WrapModule(wrapModule)
  | WrapReasonComponent(wrapReasonComponent)
  | WrapReasonValue(wrapReasonValue)
  | WrapVariant(wrapVariant);

type genTypeKind =
  | NoGenType
  | GenType
  | GenTypeOpaque;

type translation = {
  dependencies: list(Dependencies.t),
  codeItems: list(t),
};

let combineTranslations = (translations: list(translation)): translation =>
  translations
  |> List.map(({dependencies, codeItems}) => (dependencies, codeItems))
  |> List.split
  |> (
    ((dependencies, codeItems)) => {
      dependencies: dependencies |> List.concat,
      codeItems: codeItems |> List.concat,
    }
  );

let getImportTypeUniqueName = importType =>
  switch (importType) {
  | ImportComment(s) => s
  | ImportTypeAs({typeName, asTypeName, _}) =>
    typeName
    ++ (
      switch (asTypeName) {
      | None => ""
      | Some(s) => "_as_" ++ s
      }
    )
  };

let toString = (~language, codeItem) =>
  switch (codeItem) {
  | ExportType({typeName, _}) => "ExportType " ++ typeName
  | ExportVariantType({name, _}) => "ExportVariantType " ++ name
  | ImportType(importType) =>
    "ImportType " ++ getImportTypeUniqueName(importType)
  | WrapJsComponent({importAnnotation, _}) =>
    "WrapJsComponent " ++ (importAnnotation.importPath |> ImportPath.toString)
  | WrapJsValue({importAnnotation, _}) =>
    "WrapJsValue " ++ (importAnnotation.importPath |> ImportPath.toString)
  | WrapModule({moduleName, _}) =>
    "WrapModule " ++ (moduleName |> ModuleName.toString)
  | WrapReasonComponent({moduleName, _}) =>
    "WrapReasonComponent " ++ (moduleName |> ModuleName.toString)
  | WrapReasonValue({moduleName, id, typ}) =>
    "WrapReasonValue"
    ++ " id:"
    ++ Ident.name(id)
    ++ " moduleName:"
    ++ ModuleName.toString(moduleName)
    ++ " typ:"
    ++ EmitTyp.typToString(~language, typ)
  | WrapVariant({variantName, _}) => "WrapVariant " ++ variantName
  };

type attributePayload =
  | UnrecognizedPayload
  | StringPayload(string);

let rec getAttributePayload = (checkText, attributes: Typedtree.attributes) =>
  switch (attributes) {
  | [] => None
  | [({Asttypes.txt, _}, payload), ..._tl] when checkText(txt) =>
    switch (payload) {
    | PStr([
        {
          pstr_desc:
            Pstr_eval(
              {pexp_desc: Pexp_constant(Const_string(s, _)), _},
              _,
            ),
          _,
        },
      ]) =>
      Some(StringPayload(s))
    | _ => Some(UnrecognizedPayload)
    }
  | [_hd, ...tl] => getAttributePayload(checkText, tl)
  };

let hasAttribute = (checkText, attributes: Typedtree.attributes) =>
  getAttributePayload(checkText, attributes) != None;

let getGenTypeKind = (attributes: Typedtree.attributes) =>
  if (hasAttribute(tagIsGenType, attributes)) {
    GenType;
  } else if (hasAttribute(tagIsGenTypeOpaque, attributes)) {
    GenTypeOpaque;
  } else {
    NoGenType;
  };

let createFunctionType = (typeVars, argTypes, retType) =>
  if (argTypes === []) {
    retType;
  } else {
    Function({typeVars, argTypes, retType});
  };

let exportType = (~opaque, ~typeVars, ~typeName, ~comment=?, typ) => {
  opaque,
  typeVars,
  typeName,
  comment,
  typ,
};

let translateExportType = (~opaque, ~typeVars, ~typeName, ~comment=?, typ) =>
  ExportType({opaque, typeVars, typeName, comment, typ});

let variantLeafTypeName = (typeName, leafName) =>
  String.capitalize(typeName) ++ String.capitalize(leafName);

let hasGenTypeAnnotation = attributes =>
  getGenTypeKind(attributes) != NoGenType
  || attributes
  |> getAttributePayload(tagIsGenTypeImport) != None;

let rec moduleTypeHasGenTypeAnnotation =
        ({mty_desc, _}: Typedtree.module_type) =>
  switch (mty_desc) {
  | Tmty_signature(signature) => signature |> signatureHasGenTypeAnnotation
  | Tmty_ident(_)
  | Tmty_functor(_)
  | Tmty_with(_)
  | Tmty_typeof(_)
  | Tmty_alias(_) => false
  }
and moduleDeclarationHasGenTypeAnnotation =
    ({md_attributes, md_type, _}: Typedtree.module_declaration) =>
  md_attributes
  |> hasGenTypeAnnotation
  || md_type
  |> moduleTypeHasGenTypeAnnotation
and signatureItemHasGenTypeAnnotation =
    (signatureItem: Typedtree.signature_item) =>
  switch (signatureItem) {
  | {Typedtree.sig_desc: Typedtree.Tsig_type(typeDeclarations), _} =>
    typeDeclarations
    |> List.exists(dec => dec.Typedtree.typ_attributes |> hasGenTypeAnnotation)
  | {Typedtree.sig_desc: Tsig_value(valueDescription), _} =>
    valueDescription.val_attributes |> hasGenTypeAnnotation
  | {Typedtree.sig_desc: Typedtree.Tsig_module(moduleDeclaration), _} =>
    moduleDeclaration |> moduleDeclarationHasGenTypeAnnotation
  | _ => false
  }
and signatureHasGenTypeAnnotation = (signature: Typedtree.signature) =>
  signature.Typedtree.sig_items
  |> List.exists(signatureItemHasGenTypeAnnotation);

let rec structureItemHasGenTypeAnnotation =
        (structureItem: Typedtree.structure_item) =>
  switch (structureItem) {
  | {Typedtree.str_desc: Typedtree.Tstr_type(typeDeclarations), _} =>
    typeDeclarations
    |> List.exists(dec => dec.Typedtree.typ_attributes |> hasGenTypeAnnotation)
  | {Typedtree.str_desc: Tstr_value(_loc, valueBindings), _} =>
    valueBindings
    |> List.exists(vb => vb.Typedtree.vb_attributes |> hasGenTypeAnnotation)
  | {Typedtree.str_desc: Tstr_primitive(valueDescription), _} =>
    valueDescription.val_attributes |> hasGenTypeAnnotation
  | {Typedtree.str_desc: Tstr_module(moduleBinding), _} =>
    moduleBinding |> moduleBindingHasGenTypeAnnotation
  | {Typedtree.str_desc: Tstr_recmodule(moduleBindings), _} =>
    moduleBindings |> List.exists(moduleBindingHasGenTypeAnnotation)
  | _ => false
  }
and moduleBindingHasGenTypeAnnotation =
    ({mb_expr, mb_attributes, _}: Typedtree.module_binding) =>
  mb_attributes
  |> hasGenTypeAnnotation
  || (
    switch (mb_expr.mod_desc) {
    | Tmod_structure(structure) => structure |> structureHasGenTypeAnnotation
    | Tmod_ident(_)
    | Tmod_functor(_)
    | Tmod_apply(_)
    | Tmod_constraint(_)
    | Tmod_unpack(_) => false
    }
  )
and structureHasGenTypeAnnotation = (structure: Typedtree.structure) =>
  structure.str_items |> List.exists(structureItemHasGenTypeAnnotation);

/*
 * TODO: Make the types namespaced by nested Flow module.
 */
let translateConstructorDeclaration =
    (~language, ~recordGen, variantTypeName, constructorDeclaration) => {
  let constructorArgs = constructorDeclaration.Types.cd_args;
  let variantName = Ident.name(constructorDeclaration.Types.cd_id);
  let argsTranslation =
    Dependencies.translateTypeExprs(~language, constructorArgs);
  let argTypes = argsTranslation |> List.map(({Dependencies.typ, _}) => typ);
  let dependencies =
    argsTranslation
    |> List.map(({Dependencies.dependencies, _}) => dependencies)
    |> List.concat;
  /* A valid Reason identifier that we can point UpperCase JS exports to. */
  let variantTypeName = variantLeafTypeName(variantTypeName, variantName);

  let typeVars = argTypes |> TypeVars.freeOfList;

  let retType = Ident(variantTypeName, typeVars |> TypeVars.toTyp);
  let constructorTyp = createFunctionType(typeVars, argTypes, retType);
  let recordValue =
    recordGen |> Runtime.newRecordValue(~unboxed=constructorArgs == []);
  let codeItems = [
    WrapVariant({
      exportType:
        exportType(
          ~opaque=true,
          ~typeVars,
          ~typeName=variantTypeName,
          mixedOrUnknown(~language),
        ),
      constructorTyp,
      argTypes,
      variantName,
      recordValue,
    }),
  ];
  (retType, (dependencies, codeItems));
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

let translateId = (~language, ~moduleName, ~typeExpr, id): translation => {
  let typeExprTranslation =
    typeExpr |> Dependencies.translateTypeExpr(~language);
  let typeVars = typeExprTranslation.typ |> TypeVars.free;
  let typ = typeExprTranslation.typ |> abstractTheTypeParameters(~typeVars);
  let codeItems = [WrapReasonValue({moduleName, id, typ})];
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

let translateMake =
    (~language, ~propsTypeGen, ~moduleName, ~typeExpr, id): translation => {
  let typeExprTranslation =
    typeExpr
    |> Dependencies.translateTypeExpr(
         ~language,
         /* Only get the dependencies for the prop types.
            The return type is a ReasonReact component. */
         ~noFunctionReturnDependencies=true,
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
      WrapReasonComponent({
        exportType:
          exportType(
            ~opaque=false,
            ~typeVars,
            ~typeName=propsTypeName,
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
    id |> translateId(~language, ~moduleName, ~typeExpr)
  };
};

let translateValueBinding =
    (~language, ~propsTypeGen, ~moduleName, valueBinding): translation => {
  let {Typedtree.vb_pat, vb_attributes, vb_expr, _} = valueBinding;
  let typeExpr = vb_expr.exp_type;
  switch (vb_pat.pat_desc, getGenTypeKind(vb_attributes)) {
  | (Tpat_var(id, _), GenType) when Ident.name(id) == "make" =>
    id |> translateMake(~language, ~propsTypeGen, ~moduleName, ~typeExpr)
  | (Tpat_var(id, _), GenType) =>
    id |> translateId(~language, ~moduleName, ~typeExpr)
  | _ => {dependencies: [], codeItems: []}
  };
};

let translateSignatureValue =
    (
      ~language,
      ~propsTypeGen,
      ~moduleName,
      valueDescription: Typedtree.value_description,
    )
    : translation => {
  let {Typedtree.val_id, val_desc, val_attributes, _} = valueDescription;
  let typeExpr = val_desc.ctyp_type;
  switch (val_id, getGenTypeKind(val_attributes)) {
  | (id, GenType) when Ident.name(id) == "make" =>
    id |> translateMake(~language, ~propsTypeGen, ~moduleName, ~typeExpr)
  | (id, GenType) => id |> translateId(~language, ~moduleName, ~typeExpr)
  | _ => {dependencies: [], codeItems: []}
  };
};

let importAnnotationFromString = importString => {
  let name = {
    let base = importString |> Filename.basename;
    try (base |> Filename.chop_extension) {
    | Invalid_argument(_) => base
    };
  };
  let importPath = ImportPath.fromStringUnsafe(importString);
  {name, importPath};
};

/**
 * [@genType]
 * [@bs.module] external myBanner : ReasonReact.reactClass = "./MyBanner";
 */
let translatePrimitive =
    (
      ~language,
      ~moduleName,
      ~propsTypeGen,
      valueDescription: Typedtree.value_description,
    )
    : translation => {
  let valueName = valueDescription.val_id |> Ident.name;
  let typeExprTranslation =
    valueDescription.val_desc.ctyp_type
    |> Dependencies.translateTypeExpr(~language);
  let genTypeImportPayload =
    valueDescription.val_attributes |> getAttributePayload(tagIsGenTypeImport);
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
      WrapJsComponent({
        exportType:
          exportType(
            ~opaque=false,
            ~typeVars,
            ~typeName=propsTypeName,
            propsTyp,
          ),
        importAnnotation: importString |> importAnnotationFromString,
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
          importAnnotation: importString |> importAnnotationFromString,
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
    (~language, dec: Typedtree.type_declaration): translation =>
  switch (
    dec.typ_type.type_params,
    dec.typ_type.type_kind,
    getGenTypeKind(dec.typ_attributes),
  ) {
  | (typeParams, Type_record(labelDeclarations, _), GenType | GenTypeOpaque) =>
    let fieldTranslations =
      labelDeclarations
      |> List.map(({Types.ld_id, ld_type, ld_attributes, _}) => {
           let name =
             switch (ld_attributes |> getAttributePayload(tagIsGenTypeAs)) {
             | Some(StringPayload(s)) => s
             | _ => ld_id |> Ident.name
             };
           (name, ld_type |> Dependencies.translateTypeExpr(~language));
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
        translateExportType(~opaque=false, ~typeVars, ~typeName, typ),
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
            mixedOrUnknown(~language),
          ),
        ],
      }
    | Some(coreType) =>
      let typeExprTranslation =
        coreType.Typedtree.ctyp_type
        |> Dependencies.translateTypeExpr(~language);
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
          typeExprTranslation.typ,
        ),
      ];
      {dependencies: typeExprTranslation.dependencies, codeItems};
    };
  | (astTypeParams, Type_variant(constructorDeclarations), GenType)
      when !hasSomeGADTLeaf(constructorDeclarations) =>
    let variantTypeName = Ident.name(dec.typ_id);
    let resultTypesDepsAndVariantLeafBindings = {
      let recordGen = Runtime.recordGen();
      List.map(
        constructorDeclaration =>
          translateConstructorDeclaration(
            ~language,
            ~recordGen,
            variantTypeName,
            constructorDeclaration,
          ),
        constructorDeclarations,
      );
    };
    let (resultTypes, depsAndVariantLeafBindings) =
      List.split(resultTypesDepsAndVariantLeafBindings);
    let (listListDeps, listListItems) =
      List.split(depsAndVariantLeafBindings);
    let deps = List.concat(listListDeps);
    let items = List.concat(listListItems);
    let typeParams = TypeVars.(astTypeParams |> extract |> toTyp);
    let unionType =
      ExportVariantType({
        typeParams,
        leafTypes: resultTypes,
        name: variantTypeName,
      });
    {dependencies: deps, codeItems: List.append(items, [unionType])};
  | _ => {dependencies: [], codeItems: []}
  };

let rec translateStructItem =
        (~config, ~propsTypeGen, ~moduleName, structItem): translation =>
  switch (structItem) {
  | {Typedtree.str_desc: Typedtree.Tstr_type(typeDeclarations), _} =>
    typeDeclarations
    |> List.map(translateTypeDeclaration(~language=config.language))
    |> combineTranslations

  | {Typedtree.str_desc: Tstr_value(_loc, valueBindings), _} =>
    valueBindings
    |> List.map(
         translateValueBinding(
           ~language=config.language,
           ~propsTypeGen,
           ~moduleName,
         ),
       )
    |> combineTranslations

  | {Typedtree.str_desc: Tstr_primitive(valueDescription), _} =>
    /* external declaration */
    valueDescription
    |> translatePrimitive(
         ~language=config.language,
         ~moduleName,
         ~propsTypeGen,
       )

  | {Typedtree.str_desc: Tstr_module(moduleBinding), _} =>
    moduleBinding
    |> translateModuleBinding(~config, ~moduleName, ~propsTypeGen)

  | {Typedtree.str_desc: Tstr_recmodule(moduleBindings), _} =>
    moduleBindings
    |> List.map(translateModuleBinding(~config, ~moduleName, ~propsTypeGen))
    |> combineTranslations

  | _ => {dependencies: [], codeItems: []}
  }
and translateStructure =
    (~config, ~propsTypeGen, ~moduleName, structure): list(translation) =>
  structure.Typedtree.str_items
  |> List.map(structItem =>
       structItem |> translateStructItem(~config, ~propsTypeGen, ~moduleName)
     )
and translateModuleBinding =
    (
      ~config,
      ~moduleName,
      ~propsTypeGen,
      {mb_id, mb_expr, mb_attributes, _}: Typedtree.module_binding,
    )
    : translation =>
  switch (mb_expr.mod_desc) {
  | Tmod_structure(structure) =>
    let isAnnotated = mb_attributes |> hasGenTypeAnnotation;
    let {dependencies, codeItems} =
      structure
      |> translateStructure(~config, ~propsTypeGen, ~moduleName)
      |> combineTranslations;
    {
      dependencies,
      codeItems: [
        WrapModule({
          moduleName: mb_id |> Ident.name |> ModuleName.fromStringUnsafe,
          isAnnotated,
          codeItems,
        }),
      ],
    };

  | Tmod_ident(_)
  | Tmod_functor(_)
  | Tmod_apply(_)
  | Tmod_constraint(_)
  | Tmod_unpack(_) => {dependencies: [], codeItems: []}
  };

let rec translateModuleDeclaration =
        (
          ~config,
          ~propsTypeGen,
          ~moduleName,
          {md_id, md_attributes, md_type, _}: Typedtree.module_declaration,
        ) =>
  switch (md_type.mty_desc) {
  | Tmty_signature(signature) =>
    let isAnnotated = md_attributes |> hasGenTypeAnnotation;
    let {dependencies, codeItems} =
      signature
      |> translateSignature(~config, ~propsTypeGen, ~moduleName)
      |> combineTranslations;
    {
      dependencies,
      codeItems: [
        WrapModule({
          moduleName: md_id |> Ident.name |> ModuleName.fromStringUnsafe,
          isAnnotated,
          codeItems,
        }),
      ],
    };
  | Tmty_ident(_)
  | Tmty_functor(_)
  | Tmty_with(_)
  | Tmty_typeof(_)
  | Tmty_alias(_) => {dependencies: [], codeItems: []}
  }
and translateSignatureItem =
    (~config, ~propsTypeGen, ~moduleName, signatureItem): translation =>
  switch (signatureItem) {
  | {Typedtree.sig_desc: Typedtree.Tsig_type(typeDeclarations), _} =>
    typeDeclarations
    |> List.map(translateTypeDeclaration(~language=config.language))
    |> combineTranslations

  | {Typedtree.sig_desc: Tsig_value(valueDescription), _} =>
    if (valueDescription.val_prim != []) {
      valueDescription
      |> translatePrimitive(
           ~language=config.language,
           ~moduleName,
           ~propsTypeGen,
         );
    } else {
      valueDescription
      |> translateSignatureValue(
           ~language=config.language,
           ~propsTypeGen,
           ~moduleName,
         );
    }

  | {Typedtree.sig_desc: Typedtree.Tsig_module(moduleDeclaration), _} =>
    moduleDeclaration
    |> translateModuleDeclaration(~config, ~propsTypeGen, ~moduleName)

  | _ => {dependencies: [], codeItems: []}
  }
and translateSignature =
    (~config, ~propsTypeGen, ~moduleName, signature): list(translation) =>
  signature.Typedtree.sig_items
  |> List.map(signatureItem =>
       signatureItem
       |> translateSignatureItem(~config, ~propsTypeGen, ~moduleName)
     );

let typePathToImport = (~config, ~outputFileRelative, ~resolver, typePath) =>
  switch (typePath) {
  | Path.Pident(id) when Ident.name(id) == "list" => [
      ImportTypeAs({
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
  | Path.Pident(_) => []
  | Pdot(Papply(_, _), _, _)
  | Papply(_, _) => [ImportComment("// Cannot import type with Papply")]

  | Pdot(_) =>
    let rec getOuterModuleName = path =>
      switch (path) {
      | Path.Pident(id) => id |> Ident.name |> ModuleName.fromStringUnsafe
      | Pdot(path1, _, _) => path1 |> getOuterModuleName
      | Papply(_, p2) => p2 |> getOuterModuleName
      };
    let rec removeOuterModule = path =>
      switch (path) {
      | Path.Pident(_) => path
      | Pdot(Path.Pident(_), s, _) => Path.Pident(s |> Ident.create)
      | Pdot(path1, s, pos) => Pdot(path1 |> removeOuterModule, s, pos)
      | Papply(_, p2) => p2 |> removeOuterModule
      };
    let moduleName = typePath |> getOuterModuleName;
    let typeName =
      typePath |> removeOuterModule |> Dependencies.typePathToName;
    let nameFromPath = Dependencies.typePathToName(typePath);
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

let importTypeCompare = (i1, i2) =>
  compare(i1 |> getImportTypeUniqueName, i2 |> getImportTypeUniqueName);

let translateDependencies =
    (~config, ~outputFileRelative, ~resolver, dependencies): list(t) => {
  let dependencyToImportType = dependency =>
    switch (dependency) {
    | Dependencies.TypeAtPath(p) =>
      typePathToImport(~config, ~outputFileRelative, ~resolver, p)
    };
  dependencies
  |> List.map(dependencyToImportType)
  |> List.concat
  |> List.sort_uniq(importTypeCompare)
  |> List.map(importType => ImportType(importType));
};