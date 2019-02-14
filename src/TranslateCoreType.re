open GenTypeCommon;

open! TranslateTypeExprFromTypes;

let removeOption = (~label, coreType: Typedtree.core_type) =>
  switch (coreType.ctyp_desc) {
  | Ttyp_constr(Path.Pident(id), _, [t])
      /* This has a different representation in 4.03+ */
      when Ident.name(id) == "option" && label != "" && label.[0] == '?' =>
    Some((String.sub(label, 1, String.length(label) - 1), t))
  | Ttyp_constr(Pdot(Path.Pident(nameSpace), id, _), _, [t])
      /* This has a different representation in 4.03+ */
      when
        Ident.name(nameSpace) == "FB"
        && id == "option"
        && label != ""
        && label.[0] == '?' =>
    Some((String.sub(label, 1, String.length(label) - 1), t))
  | _ => None
  };

type processVariant = {
  noPayloads: list((string, Typedtree.attributes)),
  payloads: list((string, Typedtree.attributes, Typedtree.core_type)),
  unknowns: list(string),
};

let processVariant = rowFields => {
  let rec loop = (~noPayloads, ~payloads, ~unknowns, fields) =>
    switch (fields) {
    | [
        Typedtree.Ttag(
          label,
          attributes,
          _,
          /* only variants with no payload */ [],
        ),
        ...otherFields,
      ] =>
      otherFields
      |> loop(
           ~noPayloads=[(label, attributes), ...noPayloads],
           ~payloads,
           ~unknowns,
         )
    | [Ttag(label, attributes, _, [payload]), ...otherFields] =>
      otherFields
      |> loop(
           ~noPayloads,
           ~payloads=[(label, attributes, payload), ...payloads],
           ~unknowns,
         )
    | [Ttag(_, _, _, [_, _, ..._]) | Tinherit(_), ...otherFields] =>
      otherFields
      |> loop(~noPayloads, ~payloads, ~unknowns=["Tinherit", ...unknowns])
    | [] => {
        noPayloads: noPayloads |> List.rev,
        payloads: payloads |> List.rev,
        unknowns: unknowns |> List.rev,
      }
    };
  rowFields |> loop(~noPayloads=[], ~payloads=[], ~unknowns=[]);
};

let rec translateArrowType =
        (
          ~config,
          ~typeVarsGen,
          ~noFunctionReturnDependencies=false,
          ~typeEnv,
          ~revArgDeps,
          ~revArgs,
          coreType: Typedtree.core_type,
        ) =>
  switch (coreType.ctyp_desc) {
  | Ttyp_arrow("", coreType1, coreType2) =>
    let {dependencies, typ} =
      coreType1 |> translateCoreType_(~config, ~typeVarsGen, ~typeEnv, _);
    let nextRevDeps = List.rev_append(dependencies, revArgDeps);
    coreType2
    |> translateArrowType(
         ~config,
         ~typeVarsGen,
         ~noFunctionReturnDependencies,
         ~typeEnv,
         ~revArgDeps=nextRevDeps,
         ~revArgs=[(Nolabel, typ), ...revArgs],
       );
  | Ttyp_arrow(label, coreType1, coreType2) =>
    let asLabel =
      switch (coreType.ctyp_attributes |> Annotation.getAttributeRenaming) {
      | Some(s) => s
      | None => ""
      };
    switch (coreType1 |> removeOption(~label)) {
    | None =>
      let {dependencies, typ: typ1} =
        coreType1 |> translateCoreType_(~config, ~typeVarsGen, ~typeEnv);
      let nextRevDeps = List.rev_append(dependencies, revArgDeps);
      coreType2
      |> translateArrowType(
           ~config,
           ~typeVarsGen,
           ~noFunctionReturnDependencies,
           ~typeEnv,
           ~revArgDeps=nextRevDeps,
           ~revArgs=[
             (Label(asLabel == "" ? label : asLabel), typ1),
             ...revArgs,
           ],
         );
    | Some((lbl, t1)) =>
      let {dependencies, typ: typ1} =
        t1 |> translateCoreType_(~config, ~typeVarsGen, ~typeEnv);
      let nextRevDeps = List.rev_append(dependencies, revArgDeps);
      coreType2
      |> translateArrowType(
           ~config,
           ~typeVarsGen,
           ~noFunctionReturnDependencies,
           ~typeEnv,
           ~revArgDeps=nextRevDeps,
           ~revArgs=[
             (OptLabel(asLabel == "" ? lbl : asLabel), typ1),
             ...revArgs,
           ],
         );
    };
  | _ =>
    let {dependencies, typ: retType} =
      coreType |> translateCoreType_(~config, ~typeVarsGen, ~typeEnv);
    let allDeps =
      List.rev_append(
        revArgDeps,
        noFunctionReturnDependencies ? [] : dependencies,
      );

    let labeledConvertableTypes = revArgs |> List.rev;
    let argTypes = labeledConvertableTypes |> NamedArgs.group;

    let functionType =
      Function({argTypes, retType, typeVars: [], uncurried: false});

    {dependencies: allDeps, typ: functionType};
  }
and translateCoreType_ =
    (
      ~config,
      ~typeVarsGen,
      ~noFunctionReturnDependencies=false,
      ~typeEnv,
      coreType: Typedtree.core_type,
    ) =>
  switch (coreType.ctyp_desc) {
  | Ttyp_alias(ct, _) =>
    ct
    |> translateCoreType_(
         ~config,
         ~typeVarsGen,
         ~noFunctionReturnDependencies=false,
         ~typeEnv,
       )

  | Ttyp_constr(
      Pdot(Pident({name: "Js", _}), "t", _) as path,
      _,
      [
        {
          ctyp_desc:
            Ttyp_object(tObj, closedFlag) |
            Ttyp_alias({ctyp_desc: Ttyp_object(tObj, closedFlag)}, _),
          _,
        },
      ],
    ) =>
    let getFieldType = ((name, _attibutes, t)) => (
      name,
      name |> Runtime.isMutableObjectField ?
        {dependencies: [], typ: Ident("", [])} :
        t |> translateCoreType_(~config, ~typeVarsGen, ~typeEnv),
    );
    let fieldsTranslations = tObj |> List.map(getFieldType);
    translateConstr(
      ~config,
      ~fieldsTranslations,
      ~closedFlag=closedFlag == Closed ? Closed : Open,
      ~paramsTranslation=[],
      ~path,
      ~typeEnv,
    );

  | Ttyp_constr(path, _, typeParams) =>
    let paramsTranslation =
      typeParams |> translateCoreTypes_(~config, ~typeVarsGen, ~typeEnv);
    TranslateTypeExprFromTypes.translateConstr(
      ~config,
      ~fieldsTranslations=[],
      ~closedFlag=Closed,
      ~paramsTranslation,
      ~path,
      ~typeEnv,
    );

  | Ttyp_poly(_, t) =>
    t
    |> translateCoreType_(
         ~config,
         ~typeVarsGen,
         ~noFunctionReturnDependencies,
         ~typeEnv,
       )

  | Ttyp_arrow(_) =>
    coreType
    |> translateArrowType(
         ~config,
         ~typeVarsGen,
         ~noFunctionReturnDependencies,
         ~typeEnv,
         ~revArgDeps=[],
         ~revArgs=[],
       )

  | Ttyp_tuple(listExp) =>
    let innerTypesTranslation =
      listExp |> translateCoreTypes_(~config, ~typeVarsGen, ~typeEnv);
    let innerTypes = innerTypesTranslation |> List.map(({typ, _}) => typ);
    let innerTypesDeps =
      innerTypesTranslation
      |> List.map(({dependencies, _}) => dependencies)
      |> List.concat;

    let tupleType = Tuple(innerTypes);

    {dependencies: innerTypesDeps, typ: tupleType};

  | Ttyp_var(s) => {dependencies: [], typ: TypeVar(s)}

  | Ttyp_variant(rowFields, _, _) =>
    switch (rowFields |> processVariant) {
    | {noPayloads, payloads, unknowns: []} =>
      let noPayloads =
        noPayloads
        |> List.map(((label, _attibutes)) =>
             {label, labelJS: StringLabel(label)}
           );
      let payloadsTranslations =
        payloads
        |> List.map(((label, attributes, payload)) =>
             (
               label,
               attributes,
               payload |> translateCoreType_(~config, ~typeVarsGen, ~typeEnv),
             )
           );
      let payloads =
        payloadsTranslations
        |> List.map(((label, _attributes, translation)) => {
             let numArgs = 1;
             (
               {label, labelJS: StringLabel(label)},
               numArgs,
               translation.typ,
             );
           });
      let typ = createVariant(~noPayloads, ~payloads, ~polymorphic=true);
      let dependencies =
        payloadsTranslations
        |> List.map(((_, _, {dependencies, _})) => dependencies)
        |> List.concat;
      {dependencies, typ};

    | _ => {dependencies: [], typ: mixedOrUnknown(~config)}
    }

  | Ttyp_package({pack_path}) =>
    switch (typeEnv |> TypeEnv.lookupModuleTypeSignature(~path=pack_path)) {
    | Some(signature) =>
      let (dependencies, typ) =
        signature.sig_type
        |> signatureToRecordType(~config, ~typeVarsGen, ~typeEnv);
      {dependencies, typ};
    | None => {dependencies: [], typ: mixedOrUnknown(~config)}
    }

  | Ttyp_any
  | Ttyp_class(_)
  | Ttyp_object(_) => {dependencies: [], typ: mixedOrUnknown(~config)}
  }
and translateCoreTypes_ =
    (~config, ~typeVarsGen, ~typeEnv, typeExprs): list(translation) =>
  typeExprs |> List.map(translateCoreType_(~config, ~typeVarsGen, ~typeEnv));

let translateCoreType =
    (~config, ~noFunctionReturnDependencies=?, ~typeEnv, coreType) => {
  let typeVarsGen = GenIdent.createTypeVarsGen();
  let translation =
    coreType
    |> translateCoreType_(
         ~config,
         ~typeVarsGen,
         ~noFunctionReturnDependencies?,
         ~typeEnv,
       );

  if (Debug.dependencies^) {
    translation.dependencies
    |> List.iter(dep =>
         logItem("Dependency: %s\n", dep |> Dependencies.typePathToName)
       );
  };
  translation;
};