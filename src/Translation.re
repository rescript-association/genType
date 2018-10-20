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

let translateExportType =
    (~opaque, ~typeVars, ~typeName, ~typeEnv, typ): CodeItem.t => {
  let resolvedTypeName = typeName |> TypeEnv.addModulePath(~typeEnv);
  ExportType({opaque, typeVars, resolvedTypeName, typ});
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
    CodeItem.WrapVariantLeaf({
      exportType: {
        opaque: true,
        typeVars,
        resolvedTypeName: variantTypeNameResolved,
        typ: mixedOrUnknown(~language),
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
  | TypeVar(_) => typ
  };

let translateValue = (~language, ~fileName, ~typeEnv, ~typeExpr, name): t => {
  let typeExprTranslation =
    typeExpr |> Dependencies.translateTypeExpr(~language, ~typeEnv);
  let typeVars = typeExprTranslation.typ |> TypeVars.free;
  let typ = typeExprTranslation.typ |> abstractTheTypeParameters(~typeVars);
  let resolvedName = name |> TypeEnv.addModulePath(~typeEnv);

  /* Access path for the value in the module.
     I can be the value name if the module is not nested.
     Or TopLevelModule[x][y] if accessing a value in a doubly nested module */
  let valueAccessPath =
    typeEnv |> TypeEnv.getValueAccessPath(~name=resolvedName);

  let codeItems = [
    CodeItem.WrapReasonValue({fileName, resolvedName, valueAccessPath, typ}),
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

let translateComponent = (~language, ~fileName, ~typeEnv, ~typeExpr, name): t => {
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
      CodeItem.WrapReasonComponent({
        exportType: {
          opaque: false,
          typeVars,
          resolvedTypeName: propsTypeName,
          typ: propsType,
        },
        fileName,
        moduleName,
        propsTypeName,
        componentType,
        typ,
      }),
    ];
    {dependencies: typeExprTranslation.dependencies, codeItems};

  | _ =>
    /* not a component: treat make as a normal function */
    name |> translateValue(~language, ~fileName, ~typeEnv, ~typeExpr)
  };
};

let translateValueBinding =
    (~language, ~moduleItemGen, ~fileName, ~typeEnv, valueBinding): t => {
  let {Typedtree.vb_pat, vb_attributes, vb_expr, _} = valueBinding;
  let moduleItem = moduleItemGen |> Runtime.newModuleItem;
  typeEnv |> TypeEnv.updateModuleItem(~moduleItem);
  let typeExpr = vb_expr.exp_type;
  switch (vb_pat.pat_desc, Annotation.getGenTypeKind(vb_attributes)) {
  | (Tpat_var(id, _), GenType) when Ident.name(id) == "make" =>
    id
    |> Ident.name
    |> translateComponent(~language, ~fileName, ~typeEnv, ~typeExpr)
  | (Tpat_var(id, _), GenType) =>
    id
    |> Ident.name
    |> translateValue(~language, ~fileName, ~typeEnv, ~typeExpr)
  | _ => {dependencies: [], codeItems: []}
  };
};

let translateSignatureValue =
    (
      ~language,
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
    |> translateComponent(~language, ~fileName, ~typeEnv, ~typeExpr)
  | (id, GenType) =>
    id
    |> Ident.name
    |> translateValue(~language, ~fileName, ~typeEnv, ~typeExpr)
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
      ~fileName,
      ~typeEnv,
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
              |> List.map(({optional, typ} as field) =>
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
      CodeItem.WrapJsComponent({
        exportType: {
          opaque: false,
          typeVars,
          resolvedTypeName: propsTypeName,
          typ: propsTyp,
        },
        importAnnotation:
          importString |> Annotation.importAnnotationFromString,
        childrenTyp,
        propsFields,
        propsTypeName,
        fileName,
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
          fileName,
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
    (
      ~language,
      ~typeEnv,
      ~genTypeKind: CodeItem.genTypeKind,
      dec: Typedtree.type_declaration,
    )
    : t => {
  typeEnv |> TypeEnv.newType(~name=dec.typ_id |> Ident.name);

  switch (dec.typ_type.type_params, dec.typ_type.type_kind, genTypeKind) {
  | (typeParams, Type_record(labelDeclarations, _), GenType | GenTypeOpaque) =>
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
        | Array(t, _) => t |> isOpaque
        | Enum(_) => false
        | Function(_) => false
        | GroupOfLabeledArgs(_) => true
        | Ident(_) => !(typ == booleanT || typ == numberT || typ == stringT)
        | Nullable(t) => t |> isOpaque
        | Object(_) => false
        | Option(t) => t |> isOpaque
        | Record(_) => false
        | TypeVar(_) => true
        };
      let opaque =
        genTypeKind == GenTypeOpaque || typeExprTranslation.typ |> isOpaque;
      let typ =
        switch (dec.typ_manifest, typeExprTranslation.typ) {
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
        translateExportType(~opaque, ~typeVars, ~typeName, ~typeEnv, typ),
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
      variantTypeName |> TypeEnv.addModulePath(~typeEnv);
    let unionType =
      CodeItem.ExportVariantType({
        typeParams,
        variants,
        name: variantTypeNameResolved,
      });
    {dependencies: deps, codeItems: List.append(items, [unionType])};

  | ([], Type_abstract, NoGenType) =>
    switch (
      dec.typ_attributes
      |> Annotation.getAttributePayload(Annotation.tagIsGenTypeImport)
    ) {
    | Some(StringPayload(importString)) =>
      let nameWithModulePath =
        dec.typ_id |> Ident.name |> TypeEnv.addModulePath(~typeEnv);
      let (typeName, asTypeName) =
        switch (
          dec.typ_attributes
          |> Annotation.getAttributePayload(Annotation.tagIsGenTypeAs)
        ) {
        | Some(StringPayload(asString)) => (
            asString,
            Some(nameWithModulePath),
          )
        | _ => (nameWithModulePath, None)
        };

      let codeItems = [
        CodeItem.ImportType(
          ImportTypeAs({
            typeName,
            asTypeName,
            importPath: importString |> ImportPath.fromStringUnsafe,
            cmtFile: None,
          }),
        ),
      ];
      {dependencies: [], codeItems};
    | _ => {dependencies: [], codeItems: []}
    }

  | _ => {dependencies: [], codeItems: []}
  };
};

let translateTypeDeclarations =
    (~language, ~typeEnv, typeDeclarations: list(Typedtree.type_declaration)) => {
  let genTypeKind =
    switch (typeDeclarations) {
    | [dec, ..._] => dec.typ_attributes |> Annotation.getGenTypeKind
    | [] => NoGenType
    };
  typeDeclarations
  |> List.map(translateTypeDeclaration(~language, ~typeEnv, ~genTypeKind))
  |> combine;
};

let rec translateStructItem =
        (~config, ~moduleItemGen, ~fileName, ~typeEnv, structItem): t =>
  switch (structItem) {
  | {Typedtree.str_desc: Typedtree.Tstr_type(typeDeclarations), _} =>
    typeDeclarations
    |> translateTypeDeclarations(~language=config.language, ~typeEnv)

  | {Typedtree.str_desc: Tstr_value(_loc, valueBindings), _} =>
    valueBindings
    |> List.map(
         translateValueBinding(
           ~language=config.language,
           ~moduleItemGen,
           ~fileName,
           ~typeEnv,
         ),
       )
    |> combine

  | {Typedtree.str_desc: Tstr_primitive(valueDescription), _} =>
    /* external declaration */
    valueDescription
    |> translatePrimitive(~language=config.language, ~fileName, ~typeEnv)

  | {Typedtree.str_desc: Tstr_module(moduleBinding), _} =>
    moduleBinding
    |> translateModuleBinding(~config, ~fileName, ~typeEnv, ~moduleItemGen)
  | {Typedtree.str_desc: Tstr_recmodule(moduleBindings), _} =>
    moduleBindings
    |> List.map(
         translateModuleBinding(~config, ~fileName, ~typeEnv, ~moduleItemGen),
       )
    |> combine

  | _ => {dependencies: [], codeItems: []}
  }
and translateStructure = (~config, ~fileName, ~typeEnv, structure): list(t) => {
  let moduleItemGen = Runtime.moduleItemGen();
  structure.Typedtree.str_items
  |> List.map(structItem =>
       structItem
       |> translateStructItem(~config, ~moduleItemGen, ~fileName, ~typeEnv)
     );
}
and translateModuleBinding =
    (
      ~config,
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
    let {dependencies, codeItems} =
      structure
      |> translateStructure(
           ~config,
           ~fileName,
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
          ~fileName,
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
           ~fileName,
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
    (~config, ~moduleItemGen, ~fileName, ~typeEnv, signatureItem): t =>
  switch (signatureItem) {
  | {Typedtree.sig_desc: Typedtree.Tsig_type(typeDeclarations), _} =>
    typeDeclarations
    |> translateTypeDeclarations(~language=config.language, ~typeEnv)

  | {Typedtree.sig_desc: Tsig_value(valueDescription), _} =>
    if (valueDescription.val_prim != []) {
      valueDescription
      |> translatePrimitive(~language=config.language, ~fileName, ~typeEnv);
    } else {
      let moduleItem = moduleItemGen |> Runtime.newModuleItem;
      typeEnv |> TypeEnv.updateModuleItem(~moduleItem);
      valueDescription
      |> translateSignatureValue(
           ~language=config.language,
           ~fileName,
           ~typeEnv,
         );
    }

  | {Typedtree.sig_desc: Typedtree.Tsig_module(moduleDeclaration), _} =>
    let moduleItem = moduleItemGen |> Runtime.newModuleItem;
    typeEnv |> TypeEnv.updateModuleItem(~moduleItem);
    moduleDeclaration
    |> translateModuleDeclaration(~config, ~fileName, ~typeEnv);

  | _ => {dependencies: [], codeItems: []}
  }
and translateSignature = (~config, ~fileName, ~typeEnv, signature): list(t) => {
  let moduleItemGen = Runtime.moduleItemGen();
  signature.Typedtree.sig_items
  |> List.map(signatureItem =>
       signatureItem
       |> translateSignatureItem(~config, ~moduleItemGen, ~fileName, ~typeEnv)
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