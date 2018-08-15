open GenFlowCommon;

type importType =
  | ImportComment(string)
  | ImportAsFrom(string, option(string), ImportPath.t);

type exportType = {
  opaque: bool,
  typeParams: list(typ),
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
  converter,
};

type externalReactClass = {
  componentName: string,
  importPath: ImportPath.t,
};

type valueBinding = {
  moduleName: ModuleName.t,
  id: Ident.t,
  typ,
  converter,
};

type t =
  | ImportType(importType)
  | ExternalReactClass(externalReactClass)
  | ValueBinding(valueBinding)
  | ConstructorBinding(
      exportType,
      typ,
      list(convertableType),
      string,
      Runtime.recordValue,
    )
  | ComponentBinding(componentBinding)
  | ExportType(exportType)
  | ExportVariantType(exportVariantType);

type priority =
  | Import
  | Binding
  | Export;

type genFlowKind =
  | NoGenFlow
  | GenFlow
  | GenFlowOpaque;

let rec converterToString = converter =>
  switch (converter) {
  | Unit => "unit"
  | Identity => "id"
  | OptionalArgument(c) => "optionalArgument(" ++ converterToString(c) ++ ")"
  | Option(c) => "option(" ++ converterToString(c) ++ ")"
  | Fn((groupedArgConverters, c)) =>
    let labelToString = label =>
      switch (label) {
      | Nolabel => "_"
      | Label(_) => "~l"
      | OptLabel(l) => "~?" ++ l
      };
    "fn("
    ++ (
      groupedArgConverters
      |> List.map(groupedArgConverter =>
           switch (groupedArgConverter) {
           | ArgConverter(label, conv) =>
             "("
             ++ labelToString(label)
             ++ ":"
             ++ converterToString(conv)
             ++ ")"
           | GroupConverter(groupConverters) =>
             "{|"
             ++ (
               groupConverters
               |> List.map(((s, argConverter)) =>
                    s ++ ":" ++ converterToString(argConverter)
                  )
               |> String.concat(", ")
             )
             ++ "|}"
           }
         )
      |> String.concat(", ")
    )
    ++ " -> "
    ++ converterToString(c)
    ++ ")";
  };

let toString = (~language, codeItem) =>
  switch (codeItem) {
  | ImportType(_) => "ImportType"
  | ExternalReactClass(_) => "ExternalReactClass"
  | ValueBinding({moduleName, id, typ, converter}) =>
    "ValueBinding"
    ++ " moduleName:"
    ++ ModuleName.toString(moduleName)
    ++ " id:"
    ++ Ident.name(id)
    ++ " typ:"
    ++ EmitTyp.typToString(~language, typ)
    ++ " converter:"
    ++ converterToString(converter)
  | ConstructorBinding(_) => "ConstructorBinding"
  | ComponentBinding(_) => "ComponentBinding"
  | ExportType(_) => "ExportType"
  | ExportVariantType(_) => "ExportVariantType"
  };

let rec hasAttribute = (searchText, attributes) =>
  switch (attributes) {
  | [] => false
  | [({Asttypes.txt, _}, _), ..._tl] when txt == searchText => true
  | [_hd, ...tl] => hasAttribute(searchText, tl)
  };

let getGenFlowKind = attrs =>
  if (hasAttribute(tagSearch, attrs)) {
    GenFlow;
  } else if (hasAttribute(tagSearchOpaque, attrs)) {
    GenFlowOpaque;
  } else {
    NoGenFlow;
  };

module TypeVars = {
  /**
   * Extracts type variables from dependencies.
   */
  let extractOne = (~typeVarsGen, soFar, typ) =>
    switch (typ) {
    | {Types.id, desc: Tvar(None), _} =>
      let typeName = GenIdent.jsTypeNameForAnonymousTypeID(~typeVarsGen, id);
      [(typeName, id), ...soFar];
    | {id, desc: Tvar(Some(s)), _} =>
      let typeName = s;
      [(typeName, id), ...soFar];
    | _ => soFar
    };

  /*
   * Utility for extracting results of compiling to output.
   * Input:
   *
   *     [
   *       ([dep, dep], [itm, itm]),
   *       ([dep, dep], [itm, itm])
   *     ]
   *
   * Output:
   *
   * List.merge
   *     ([dep, dep, dep, dep], [itm, itm, itm, itm])
   */

  let extract = typeParams => {
    let typeVarsGen = GenIdent.createTypeVarsGen();
    typeParams |> List.fold_left(extractOne(~typeVarsGen), []) |> List.rev;
  };

  let names = freeTypeVars => List.map(((name, _id)) => name, freeTypeVars);
  let toFlow = freeTypeVars =>
    List.map(((name, _id)) => Ident(name, []), freeTypeVars);
};

let createFunctionType = (generics, argConvertableTypes, resultType) =>
  if (argConvertableTypes === []) {
    resultType;
  } else {
    let args = List.map(((_, flowTyp)) => flowTyp, argConvertableTypes);
    Arrow(generics, args, resultType);
  };

let exportType = (~opaque, typeParams, ~typeName, ~comment=?, typ) => {
  opaque,
  typeParams,
  typeName,
  comment,
  typ,
};

let codeItemForExportType = (~opaque, typeParams, ~typeName, ~comment=?, typ) =>
  ExportType({opaque, typeParams, typeName, comment, typ});

let variantLeafTypeName = (typeName, leafName) =>
  String.capitalize(typeName) ++ String.capitalize(leafName);

/*
 * TODO: Make the types namespaced by nested Flow module.
 */
let codeItemsFromConstructorDeclaration =
    (~language, variantTypeName, constructorDeclaration, ~recordGen) => {
  let constructorArgs = constructorDeclaration.Types.cd_args;
  let variantName = Ident.name(constructorDeclaration.Types.cd_id);
  let conversionPlans =
    Dependencies.typeExprsToConversion(~language, constructorArgs);
  let convertableTypes =
    conversionPlans
    |> List.map(({Dependencies.convertableType, _}) => convertableType);
  let dependencies =
    conversionPlans
    |> List.map(({Dependencies.dependencies, _}) => dependencies)
    |> List.concat;
  /* A valid Reason identifier that we can point UpperCase JS exports to. */
  let variantTypeName = variantLeafTypeName(variantTypeName, variantName);
  let (freeTypeVars, remainingDeps) =
    Dependencies.extractFreeTypeVars(dependencies);
  let typeVars = TypeVars.toFlow(freeTypeVars);
  let retType = Ident(variantTypeName, typeVars);
  let constructorTyp =
    createFunctionType(typeVars, convertableTypes, retType);
  let recordValue =
    recordGen |> Runtime.newRecordValue(~unboxed=constructorArgs == []);
  let codeItems = [
    ConstructorBinding(
      exportType(~opaque=true, typeVars, ~typeName=variantTypeName, any),
      constructorTyp,
      convertableTypes,
      variantName,
      recordValue,
    ),
  ];
  (retType, (remainingDeps, codeItems));
};

/* Applies type parameters to types (for all) */
let abstractTheTypeParameters = (typ, params) =>
  switch (typ) {
  | Optional(_) => typ
  | Ident(_) => typ
  | ObjectType(_) => typ
  | Arrow(_, valParams, retType) => Arrow(params, valParams, retType)
  };

let codeItemsForId = (~language, ~moduleName, ~valueBinding, id) => {
  let {Typedtree.vb_expr, _} = valueBinding;
  let typeExpr = vb_expr.exp_type;
  let {Dependencies.dependencies, convertableType: (converter, typ)} =
    typeExpr |> Dependencies.typeExprToConversion(~language);
  /*
   * We pull apart the polymorphic type variables at the binding level, but
   * not at deeper function types because we know that the Reason/OCaml type
   * system doesn't support higher ranked polymorphism, and so all type
   * variables most likely belong at the binding level.
   */
  let (freeTypeVars, remainingDeps) =
    Dependencies.extractFreeTypeVars(dependencies);
  let typeVars = TypeVars.toFlow(freeTypeVars);
  let typ = abstractTheTypeParameters(typ, typeVars);
  let codeItems = [ValueBinding({moduleName, id, typ, converter})];
  (remainingDeps, codeItems);
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

let codeItemsForMake =
    (~language, ~propsTypeGen, ~moduleName, ~valueBinding, id) => {
  let {Typedtree.vb_expr, _} = valueBinding;
  let typeExpr = vb_expr.exp_type;
  let {Dependencies.dependencies, convertableType: (converter, typ)} =
    typeExpr
    |> Dependencies.typeExprToConversion(
         ~language,
         /* Only get the dependencies for the prop types.
            The return type is a ReasonReact component. */
         ~noFunctionReturnDependencies=true,
       );
  let (_, remainingDeps) = Dependencies.extractFreeTypeVars(dependencies);
  switch (typ) {
  | Arrow(
      _,
      [propOrChildren, ...childrenOrNil],
      Ident(
        "ReasonReactcomponentSpec" | "ReactcomponentSpec" |
        "ReasonReactcomponent" |
        "Reactcomponent",
        [_state, ..._],
      ),
    ) =>
    /* Add children?:any to props type */
    let childrenField = ("children", NonMandatory, any);
    let propsTypeArguments =
      switch (childrenOrNil) {
      /* Then we only extracted a function that accepts children, no props */
      | [] => ObjectType([childrenField])
      /* Then we had both props and children. */
      | [_children, ..._] =>
        switch (propOrChildren) {
        | ObjectType(fields) => ObjectType(fields @ [childrenField])
        | _ => propOrChildren
        }
      };
    let propsTypeName = GenIdent.propsTypeName(~propsTypeGen);
    let componentType = EmitTyp.reactComponentType(~language, ~propsTypeName);

    let items = [
      ComponentBinding({
        exportType:
          exportType(
            ~opaque=false,
            [],
            ~typeName=propsTypeName,
            propsTypeArguments,
          ),
        moduleName,
        propsTypeName,
        componentType,
        converter,
      }),
    ];
    (remainingDeps, items);

  | _ =>
    /* not a component: treat make as a normal function */
    id |> codeItemsForId(~language, ~moduleName, ~valueBinding)
  };
};

let fromValueBinding = (~language, ~propsTypeGen, ~moduleName, valueBinding) => {
  let {Typedtree.vb_pat, vb_attributes, _} = valueBinding;
  switch (vb_pat.pat_desc, getGenFlowKind(vb_attributes)) {
  | (Tpat_var(id, _), GenFlow) when Ident.name(id) == "make" =>
    id
    |> codeItemsForMake(~language, ~propsTypeGen, ~moduleName, ~valueBinding)
  | (Tpat_var(id, _), GenFlow) =>
    id |> codeItemsForId(~language, ~moduleName, ~valueBinding)
  | _ => ([], [])
  };
};

/**
 * [@genFlow]
 * [@bs.module] external myBanner : ReasonReact.reactClass = "./MyBanner";
 */
let fromValueDescription =
    (~language, valueDescription: Typedtree.value_description) => {
  let componentName =
    valueDescription.val_id |> Ident.name |> String.capitalize;
  let path =
    switch (valueDescription.val_prim) {
    | [firstValPrim, ..._] => firstValPrim
    | [] => ""
    };
  let importPath = path |> ImportPath.fromStringUnsafe;
  let conversionPlan =
    valueDescription.val_desc.ctyp_type
    |> Dependencies.typeExprToConversion(~language);
  let typ = conversionPlan.convertableType |> snd;
  let genFlowKind = getGenFlowKind(valueDescription.val_attributes);
  switch (typ, genFlowKind) {
  | (Ident("ReasonReactreactClass", []), GenFlow) when path != "" => (
      [],
      [[ExternalReactClass({componentName, importPath})]],
    )
  | _ => ([], [])
  };
};

let hasSomeGADTLeaf = constructorDeclarations =>
  List.exists(
    declaration => declaration.Types.cd_res !== None,
    constructorDeclarations,
  );

let fromTypeDecl = (~language, dec: Typedtree.type_declaration) =>
  switch (
    dec.typ_type.type_params,
    dec.typ_type.type_kind,
    getGenFlowKind(dec.typ_attributes),
  ) {
  | (typeParams, Type_record(_, _), GenFlow | GenFlowOpaque) =>
    let freeTypeVarNames = TypeVars.extract(typeParams);
    let typeVars = TypeVars.toFlow(freeTypeVarNames);
    let typeName = Ident.name(dec.typ_id);
    (
      [],
      [
        codeItemForExportType(
          ~opaque=true,
          typeVars,
          ~typeName,
          ~comment="Record type not supported",
          any,
        ),
      ],
    );
  /*
   * This case includes aliasings such as:
   *
   *     type list('t) = List.t('t');
   */
  | (typeParams, Type_abstract, GenFlow | GenFlowOpaque)
  | (typeParams, Type_variant(_), GenFlowOpaque) =>
    let freeTypeVarNames = TypeVars.extract(typeParams);
    let typeVars = TypeVars.toFlow(freeTypeVarNames);
    let typeName = Ident.name(dec.typ_id);
    switch (dec.typ_manifest) {
    | None => (
        [],
        [codeItemForExportType(~opaque=true, typeVars, ~typeName, any)],
      )
    | Some(coreType) =>
      let opaque =
        switch (coreType.ctyp_desc) {
        | Ttyp_constr(
            Path.Pident({name: "int" | "bool" | "string" | "unit", _}),
            _,
            [],
          ) =>
          false
        | _ => true
        };
      let {Dependencies.dependencies, convertableType: (_converter, typ)} =
        coreType.Typedtree.ctyp_type
        |> Dependencies.typeExprToConversion(~language);
      let structureItems = [
        codeItemForExportType(~opaque, typeVars, ~typeName, typ),
      ];
      let deps =
        Dependencies.filterFreeTypeVars(freeTypeVarNames, dependencies);
      (deps, structureItems);
    };
  | (astTypeParams, Type_variant(constructorDeclarations), GenFlow)
      when !hasSomeGADTLeaf(constructorDeclarations) =>
    let variantTypeName = Ident.name(dec.typ_id);
    let resultTypesDepsAndVariantLeafBindings = {
      let recordGen = Runtime.recordGen();
      List.map(
        constructorDeclaration =>
          codeItemsFromConstructorDeclaration(
            ~language,
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
    let typeParams = TypeVars.(astTypeParams |> extract |> toFlow);
    let unionType =
      ExportVariantType({
        typeParams,
        leafTypes: resultTypes,
        name: variantTypeName,
      });
    (deps, List.append(items, [unionType]));
  | _ => ([], [])
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
      ++ ". Make sure it is also marked with @genFlow",
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
        let asTypeName = Dependencies.typePathToFlowName(typePath);
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
  switch (i1, i2) {
  | (ImportComment(s1), ImportComment(s2)) => compare(s1, s2)
  | (ImportComment(_), _) => (-1)
  | (_, ImportComment(_)) => 1
  | (ImportAsFrom(x1, y1, z1), ImportAsFrom(x2, y2, z2)) =>
    let x = compare(x1, x2);
    x != 0 ?
      x :
      {
        let y = compare(y1, y2);
        y != 0 ? y : compare(z1, z2);
      };
  };

let fromDependencies =
    (~config, ~outputFileRelative, ~resolver, dependencies): list(t) => {
  let dependencyToImportType = dependency =>
    switch (dependency) {
    | Dependencies.TypeAtPath(p) =>
      typePathToImport(~config, ~outputFileRelative, ~resolver, p)
    | JSTypeFromModule(typeName, asTypeName, importPath) =>
      ImportAsFrom(typeName, asTypeName, importPath)
    | FreeTypeVariable(s, _id) =>
      ImportComment("// Warning polymorphic type unhandled:" ++ s)
    /* TODO: Currently unused. Would be useful for injecting dependencies
     * on runtime converters that end up being used. */
    };
  dependencies
  |> List.map(dependencyToImportType)
  |> List.sort_uniq(importTypeCompare)
  |> List.map(importType => ImportType(importType));
};