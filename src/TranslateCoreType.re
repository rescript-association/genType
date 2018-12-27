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

let processVariant = rowFields => {
  let rec loop = (~noPayloads, ~payloads, fields) =>
    switch (fields) {
    | [
        Typedtree.Ttag(
          label,
          attributes,
          _,
          /* only enums with no payload */ [],
        ),
        ...otherFields,
      ] =>
      otherFields
      |> loop(~noPayloads=[(label, attributes), ...noPayloads], ~payloads)
    | [Ttag(label, _, _, payload), ...otherFields] =>
      otherFields
      |> loop(~noPayloads, ~payloads=[(label, payload), ...payloads])
    | [Tinherit(_), ...otherFields] =>
      otherFields |> loop(~noPayloads, ~payloads)
    | [] => (noPayloads |> List.rev, payloads |> List.rev)
    };
  rowFields |> loop(~noPayloads=[], ~payloads=[]);
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
      switch (
        coreType.ctyp_attributes
        |> Annotation.getAttributePayload(Annotation.tagIsGenTypeAs)
      ) {
      | Some(StringPayload(s)) => s
      | _ => ""
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

    let functionType = Function({typeVars: [], argTypes, retType});

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
  | Ttyp_var(s) => {dependencies: [], typ: TypeVar(s)}

  | Ttyp_constr(
      Pdot(Pident({name: "Js", _}), "t", _) as path,
      _,
      [{ctyp_desc: Ttyp_object(tObj, _), _}],
    ) =>
    let getFieldType = ((name, _attibutes, t)) => (
      name,
      name |> Runtime.isMutableObjectField ?
        {dependencies: [], typ: Ident("", [])} :
        t |> translateCoreType_(~config, ~typeVarsGen, ~typeEnv),
    );
    let fieldsTranslations = tObj |> List.map(getFieldType);
    translateConstr(
      ~path,
      ~paramsTranslation=[],
      ~typeEnv,
      ~fieldsTranslations,
    );

  | Ttyp_constr(path, _, typeParams) =>
    let paramsTranslation =
      typeParams |> translateCoreTypes_(~config, ~typeVarsGen, ~typeEnv);
    TranslateTypeExprFromTypes.translateConstr(
      ~path,
      ~paramsTranslation,
      ~typeEnv,
      ~fieldsTranslations=[],
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

  | Ttyp_variant(rowFields, _, _) =>
    switch (rowFields |> processVariant) {
    | (noPayloads, []) =>
      let cases =
        noPayloads
        |> List.map(((label, _attibutes)) =>
             {label, labelJS: StringLabel(label)}
           );
      let typ = cases |> createEnum(~obj=None);
      {dependencies: [], typ};

    | (noPayloads, [(label, [payload])]) =>
      let cases =
        noPayloads
        |> List.map(((label, _attibutes)) =>
             {label, labelJS: StringLabel(label)}
           );
      let payloadTranslation =
        payload |> translateCoreType_(~config, ~typeVarsGen, ~typeEnv);
      let typ =
        cases |> createEnum(~obj=Some((label, payloadTranslation.typ)));
      {dependencies: payloadTranslation.dependencies, typ};

    | _ => {dependencies: [], typ: mixedOrUnknown(~config)}
    }

  | Ttyp_alias(_)
  | Ttyp_any
  | Ttyp_class(_)
  | Ttyp_object(_)
  | Ttyp_package(_) => {dependencies: [], typ: mixedOrUnknown(~config)}
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