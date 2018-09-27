open GenFlowCommon;

type importType =
  | ImportComment(string)
  | ImportAsFrom(string, option(string), ImportPath.t);

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

type componentBinding = {
  exportType,
  moduleName: ModuleName.t,
  propsTypeName: string,
  componentType: typ,
  typ,
};

type externalReactClass = {
  componentName: string,
  importPath: ImportPath.t,
};

type valueBinding = {
  moduleName: ModuleName.t,
  id: Ident.t,
  typ,
};

type t =
  | ImportType(importType)
  | ExternalReactClass(externalReactClass)
  | ValueBinding(valueBinding)
  | ConstructorBinding(
      exportType,
      typ,
      list(typ),
      string,
      Runtime.recordValue,
    )
  | ComponentBinding(componentBinding)
  | ExportType(exportType)
  | ExportVariantType(exportVariantType);

type genFlowKind =
  | NoGenFlow
  | GenFlow
  | GenFlowOpaque;

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

let getImportTypeName = importType =>
  switch (importType) {
  | ImportComment(s) => s
  | ImportAsFrom(s, _, _) => s
  };

let toString = (~language, codeItem) =>
  switch (codeItem) {
  | ImportType(importType) => "ImportType " ++ getImportTypeName(importType)
  | ExternalReactClass(externalReactClass) =>
    "ExternalReactClass " ++ externalReactClass.componentName
  | ValueBinding({moduleName, id, typ}) =>
    "ValueBinding"
    ++ " id:"
    ++ Ident.name(id)
    ++ " moduleName:"
    ++ ModuleName.toString(moduleName)
    ++ " typ:"
    ++ EmitTyp.typToString(~language, typ)
  | ConstructorBinding(_, _, _, variantName, _) =>
    "ConstructorBinding " ++ variantName
  | ComponentBinding(componentBinding) =>
    "ComponentBinding " ++ (componentBinding.moduleName |> ModuleName.toString)
  | ExportType(exportType) => "ExportType " ++ exportType.typeName
  | ExportVariantType(exportVariantType) =>
    "ExportVariantType " ++ exportVariantType.name
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
            Pstr_eval({pexp_desc: Pexp_constant(Const_string(s, _))}, _),
        },
      ]) =>
      Some(StringPayload(s))
    | _ => Some(UnrecognizedPayload)
    }
  | [_hd, ...tl] => getAttributePayload(checkText, tl)
  };

let hasAttribute = (checkText, attributes: Typedtree.attributes) =>
  getAttributePayload(checkText, attributes) != None;

let getGenFlowKind = (attributes: Typedtree.attributes) =>
  if (hasAttribute(tagIsGenType, attributes)) {
    GenFlow;
  } else if (hasAttribute(tagIsGenFlowOpaque, attributes)) {
    GenFlowOpaque;
  } else {
    NoGenFlow;
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

/*
 * TODO: Make the types namespaced by nested Flow module.
 */
let translateConstructorDeclaration =
    (variantTypeName, constructorDeclaration, ~recordGen) => {
  let constructorArgs = constructorDeclaration.Types.cd_args;
  let variantName = Ident.name(constructorDeclaration.Types.cd_id);
  let argsTranslation = Dependencies.translateTypeExprs(constructorArgs);
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
    ConstructorBinding(
      exportType(~opaque=true, ~typeVars, ~typeName=variantTypeName, any),
      constructorTyp,
      argTypes,
      variantName,
      recordValue,
    ),
  ];
  (retType, (dependencies, codeItems));
};

/* Applies type parameters to types (for all) */
let abstractTheTypeParameters = (~typeVars, typ) =>
  switch (typ) {
  | Ident(_)
  | TypeVar(_)
  | Option(_)
  | Array(_)
  | Object(_)
  | Record(_) => typ
  | Function({argTypes, retType}) => Function({typeVars, argTypes, retType})
  };

let translateId = (~moduleName, ~typeExpr, id): translation => {
  let typeExprTranslation = typeExpr |> Dependencies.translateTypeExpr;
  let typeVars = typeExprTranslation.typ |> TypeVars.free;
  let typ = typeExprTranslation.typ |> abstractTheTypeParameters(~typeVars);
  let codeItems = [ValueBinding({moduleName, id, typ})];
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
           Some(any);
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
          "ReasonReactcomponentSpec" | "ReactcomponentSpec" |
          "ReasonReactcomponent" |
          "Reactcomponent",
          [_state, ..._],
        ),
    }) =>
    /* Add children?:any to props type */
    let propsTypeArguments =
      switch (childrenOrNil) {
      /* Then we only extracted a function that accepts children, no props */
      | [] => Object([("children", NonMandatory, any)])
      /* Then we had both props and children. */
      | [children, ..._] =>
        switch (propOrChildren) {
        | Object(fields) =>
          Object(fields @ [("children", NonMandatory, children)])
        | _ => propOrChildren
        }
      };
    let propsTypeName = GenIdent.propsTypeName(~propsTypeGen);
    let componentType = EmitTyp.reactComponentType(~language, ~propsTypeName);

    let codeItems = [
      ComponentBinding({
        exportType:
          exportType(
            ~opaque=false,
            ~typeVars,
            ~typeName=propsTypeName,
            propsTypeArguments,
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
    id |> translateId(~moduleName, ~typeExpr)
  };
};

let translateStructValue =
    (~language, ~propsTypeGen, ~moduleName, valueBinding): translation => {
  let {Typedtree.vb_pat, vb_attributes, vb_expr, _} = valueBinding;
  let typeExpr = vb_expr.exp_type;
  switch (vb_pat.pat_desc, getGenFlowKind(vb_attributes)) {
  | (Tpat_var(id, _), GenFlow) when Ident.name(id) == "make" =>
    id |> translateMake(~language, ~propsTypeGen, ~moduleName, ~typeExpr)
  | (Tpat_var(id, _), GenFlow) => id |> translateId(~moduleName, ~typeExpr)
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
  switch (val_id, getGenFlowKind(val_attributes)) {
  | (id, GenFlow) when Ident.name(id) == "make" =>
    id |> translateMake(~language, ~propsTypeGen, ~moduleName, ~typeExpr)
  | (id, GenFlow) => id |> translateId(~moduleName, ~typeExpr)
  | _ => {dependencies: [], codeItems: []}
  };
};

/**
 * [@genType]
 * [@bs.module] external myBanner : ReasonReact.reactClass = "./MyBanner";
 */
let translatePrimitive =
    (valueDescription: Typedtree.value_description): translation => {
  let componentName =
    valueDescription.val_id |> Ident.name |> String.capitalize;
  let path =
    switch (valueDescription.val_prim) {
    | [firstValPrim, ..._] => firstValPrim
    | [] => ""
    };
  let importPath = path |> ImportPath.fromStringUnsafe;
  let typeExprTranslation =
    valueDescription.val_desc.ctyp_type |> Dependencies.translateTypeExpr;
  let genFlowKind = getGenFlowKind(valueDescription.val_attributes);
  switch (typeExprTranslation.typ, genFlowKind) {
  | (Ident("ReasonReactreactClass", []), GenFlow) when path != "" => {
      dependencies: [],
      codeItems: [ExternalReactClass({componentName, importPath})],
    }
  | _ => {dependencies: [], codeItems: []}
  };
};

let hasSomeGADTLeaf = constructorDeclarations =>
  List.exists(
    declaration => declaration.Types.cd_res !== None,
    constructorDeclarations,
  );

let translateTypeDecl = (dec: Typedtree.type_declaration): translation =>
  switch (
    dec.typ_type.type_params,
    dec.typ_type.type_kind,
    getGenFlowKind(dec.typ_attributes),
  ) {
  | (typeParams, Type_record(labelDeclarations, _), GenFlow | GenFlowOpaque) =>
    let fieldTranslations =
      labelDeclarations
      |> List.map(({Types.ld_id, ld_type, ld_attributes}) => {
           let name =
             switch (ld_attributes |> getAttributePayload(tagIsGenTypeAs)) {
             | Some(StringPayload(s)) => s
             | _ => ld_id |> Ident.name
             };
           (name, ld_type |> Dependencies.translateTypeExpr);
         });
    let dependencies =
      fieldTranslations
      |> List.map(((_, {Dependencies.dependencies})) => dependencies)
      |> List.concat;
    let fields =
      fieldTranslations
      |> List.map(((name, {Dependencies.typ})) => {
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
  | (typeParams, Type_abstract, GenFlow | GenFlowOpaque)
  | (typeParams, Type_variant(_), GenFlowOpaque) =>
    let typeVars = TypeVars.extract(typeParams);
    let typeName = Ident.name(dec.typ_id);
    switch (dec.typ_manifest) {
    | None => {
        dependencies: [],
        codeItems: [
          translateExportType(~opaque=true, ~typeVars, ~typeName, any),
        ],
      }
    | Some(coreType) =>
      let typeExprTranslation =
        coreType.Typedtree.ctyp_type |> Dependencies.translateTypeExpr;
      let rec isOpaque = typ =>
        switch (typ) {
        | Ident("boolean" | "number" | "string", []) => false
        | Option(t) => t |> isOpaque
        | _ => true
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
  | (astTypeParams, Type_variant(constructorDeclarations), GenFlow)
      when !hasSomeGADTLeaf(constructorDeclarations) =>
    let variantTypeName = Ident.name(dec.typ_id);
    let resultTypesDepsAndVariantLeafBindings = {
      let recordGen = Runtime.recordGen();
      List.map(
        constructorDeclaration =>
          translateConstructorDeclaration(
            variantTypeName,
            constructorDeclaration,
            ~recordGen,
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

let typePathToImport =
    (
      ~config as {modulesMap, language},
      ~outputFileRelative,
      ~resolver,
      typePath,
    ) =>
  switch (typePath) {
  | Path.Pident(id) when Ident.name(id) == "list" =>
    ImportAsFrom(
      "list",
      None,
      ModuleName.reasonPervasives
      |> ModuleResolver.importPathForReasonModuleName(
           ~language,
           ~outputFileRelative,
           ~resolver,
           ~modulesMap,
         ),
    )

  | Path.Pident(id) =>
    ImportComment(
      "// No need to import locally visible type "
      ++ Ident.name(id)
      ++ ". Make sure it is also marked with @genType",
    )

  | Pdot(Papply(_, _), _, _)
  | Papply(_, _) => ImportComment("// Cannot import type with Papply")

  | Pdot(p, s, _pos) =>
    let moduleName =
      switch (p) {
      | Path.Pident(id) => id |> Ident.name |> ModuleName.fromStringUnsafe
      | Pdot(_, lastNameInPath, _) =>
        lastNameInPath |> ModuleName.fromStringUnsafe
      | Papply(_, _) => assert(false) /* impossible: handled above */
      };
    let typeName = s;
    ImportAsFrom(
      typeName,
      {
        let asTypeName = Dependencies.typePathToName(typePath);
        asTypeName == typeName ? None : Some(asTypeName);
      },
      moduleName
      |> ModuleResolver.importPathForReasonModuleName(
           ~language,
           ~outputFileRelative,
           ~resolver,
           ~modulesMap,
         ),
    );
  };

let importTypeCompare = (i1, i2) =>
  compare(i1 |> getImportTypeName, i2 |> getImportTypeName);

let translateDependencies =
    (~config, ~outputFileRelative, ~resolver, dependencies): list(t) => {
  let dependencyToImportType = dependency =>
    switch (dependency) {
    | Dependencies.TypeAtPath(p) =>
      typePathToImport(~config, ~outputFileRelative, ~resolver, p)
    };
  dependencies
  |> List.map(dependencyToImportType)
  |> List.sort_uniq(importTypeCompare)
  |> List.map(importType => ImportType(importType));
};