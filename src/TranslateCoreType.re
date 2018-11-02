open GenTypeCommon;

type translation = {
  dependencies: list(Dependencies.path),
  typ,
};

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

  | Ttyp_constr(Pdot(Pident({name: "FB", _}), "bool", _), _, [])
  | Ttyp_constr(Pident({name: "bool", _}), _, []) => {
      dependencies: [],
      typ: booleanT,
    }

  | Ttyp_constr(Pdot(Pident({name: "FB", _}), "int", _), _, [])
  | Ttyp_constr(Pident({name: "int", _}), _, []) => {
      dependencies: [],
      typ: numberT,
    }

  | Ttyp_constr(Pdot(Pident({name: "FB", _}), "float", _), _, [])
  | Ttyp_constr(Pident({name: "float", _}), _, []) => {
      dependencies: [],
      typ: numberT,
    }

  | Ttyp_constr(Pdot(Pident({name: "FB", _}), "string", _), _, [])
  | Ttyp_constr(Pident({name: "string", _}), _, _) => {
      dependencies: [],
      typ: stringT,
    }

  | Ttyp_constr(Pdot(Pident({name: "FB", _}), "unit", _), _, [])
  | Ttyp_constr(Pident({name: "unit", _}), _, []) => {
      dependencies: [],
      typ: unitT,
    }

  | Ttyp_constr(Pdot(Pident({name: "FB", _}), "array", _), _, [param])
  | Ttyp_constr(Pident({name: "array", _}), _, [param])
  | Ttyp_constr(
      Pdot(Pdot(Pident({name: "Js", _}), "Array", _), "t", _),
      _,
      [param],
    ) =>
    let paramTranslation =
      param |> translateCoreType_(~config, ~typeVarsGen, ~typeEnv);
    {...paramTranslation, typ: Array(paramTranslation.typ, Mutable)};

  | Ttyp_constr(
      Pdot(Pident({name: "ImmutableArray", _}), "t", _),
      _,
      [param],
    ) =>
    let paramTranslation =
      param |> translateCoreType_(~config, ~typeVarsGen, ~typeEnv);
    {...paramTranslation, typ: Array(paramTranslation.typ, Immutable)};

  | Ttyp_constr(Pdot(Pident({name: "FB", _}), "option", _), _, [param])
  | Ttyp_constr(Pident({name: "option", _}), _, [param]) =>
    let paramTranslation =
      param |> translateCoreType_(~config, ~typeVarsGen, ~typeEnv);
    {...paramTranslation, typ: Option(paramTranslation.typ)};

  | Ttyp_constr(
      Pdot(Pdot(Pident({name: "Js", _}), "Nullable", _), "t", _),
      _,
      [param],
    )
  | Ttyp_constr(Pdot(Pident({name: "Js", _}), "nullable", _), _, [param])
  | Ttyp_constr(
      Pdot(Pdot(Pident({name: "Js", _}), "Null_undefined", _), "t", _),
      _,
      [param],
    )
  | Ttyp_constr(
      Pdot(Pident({name: "Js", _}), "null_undefined", _),
      _,
      [param],
    ) =>
    let paramTranslation =
      param |> translateCoreType_(~config, ~typeVarsGen, ~typeEnv);
    {...paramTranslation, typ: Nullable(paramTranslation.typ)};

  | Ttyp_constr(
      Pdot(Pident({name: "Js", _}), "t", _),
      _,
      [{ctyp_desc: Ttyp_object(tObj, _), _}],
    ) =>
    let getFieldType = ((name, _attibutes, t)) => (
      name,
      name |> Runtime.isMutableObjectField ?
        {dependencies: [], typ: Ident("", [])} :
        t |> translateCoreType_(~config, ~typeVarsGen, ~typeEnv),
    );
    let fieldTranslations = tObj |> List.map(getFieldType);
    let dependencies =
      fieldTranslations
      |> List.map(((_, {dependencies, _})) => dependencies)
      |> List.concat;
    let rec checkMutableField = (~acc=[], fields) =>
      switch (fields) {
      | [(previousName, {typ: _, _}), (name, {typ, _}), ...rest]
          when Runtime.checkMutableObjectField(~previousName, ~name) =>
        /* The field was annotated "@bs.set" */
        rest |> checkMutableField(~acc=[(name, typ, Mutable), ...acc])
      | [(name, {typ, _}), ...rest] =>
        rest |> checkMutableField(~acc=[(name, typ, Immutable), ...acc])
      | [] => acc |> List.rev
      };
    let fields =
      fieldTranslations
      |> checkMutableField
      |> List.map(((name, typ_, mutable_)) => {
           let (optional, typ) =
             switch (typ_) {
             | Option(typ) => (Optional, typ)
             | _ => (Mandatory, typ_)
             };
           {name, optional, mutable_, typ};
         });
    let typ = Object(fields);
    {dependencies, typ};

  | Ttyp_constr(path, _, []) =>
    let resolvedPath = path |> Dependencies.resolveTypePath(~typeEnv);
    {
      dependencies: [resolvedPath],
      typ: Ident(resolvedPath |> Dependencies.typePathToName, []),
    };

  /* This type doesn't have any built in converter. But what if it was a
   * genType variant type? */
  /*
   * Built-in standard library parameterized types (aside from option) are
   * like custom parameterized types in that they don't undergo conversion,
   * and their type parameter's dependencies are tracked.  For example
   * `list(int)` will be treated just like a custom type named List that.
   * There is special treatment of TypeAtPath("list") to make sure the
   * built-in JS type defs are brought in from the right location.
   */
  | Ttyp_constr(path, _, typeParams) =>
    let paramsTranslation =
      typeParams |> translateCoreTypes_(~config, ~typeVarsGen, ~typeEnv);
    let typeArgs = paramsTranslation |> List.map(({typ, _}) => typ);
    let typeParamDeps =
      paramsTranslation
      |> List.map(({dependencies, _}) => dependencies)
      |> List.concat;
    let resolvedPath = path |> Dependencies.resolveTypePath(~typeEnv);
    {
      dependencies: [resolvedPath, ...typeParamDeps],
      typ: Ident(resolvedPath |> Dependencies.typePathToName, typeArgs),
    };

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

  | Ttyp_variant(rowFields, _, _)
      when
        rowFields
        |> List.for_all(field =>
             switch (field) {
             | Typedtree.Ttag(_, _, _, /* only enums with no payloads */ []) =>
               true
             | _ => false
             }
           ) =>
    let cases =
      rowFields
      |> List.map(field =>
           switch (field) {
           | Typedtree.Ttag(label, _attributes, _, _) => {
               label,
               labelJS: label,
             }
           | Tinherit(_) => /* impossible: checked above */ assert(false)
           }
         );
    let typ = cases |> createEnum;
    {dependencies: [], typ};

  | Ttyp_variant(_) => {dependencies: [], typ: mixedOrUnknown(~config)}

  | _ => {dependencies: [], typ: mixedOrUnknown(~config)}
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