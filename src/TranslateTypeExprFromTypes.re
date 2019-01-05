open GenTypeCommon;

type translation = {
  dependencies: list(Dependencies.path),
  typ,
};

/*
  When reading the data structures: There are structures from the `Typedtree`
  module which is the typed AST, and that AST references types from the module
  `Types` which represent the result of type checking.

  - `Typedtree` usually has record fields of the form `typ_x`
  - `Typedtree` usually has variants of the form Ttype_foo
  - `Types` usually has record fields of the form `type_x`
  - `Types` usually has variants of the form Type_foo
  - types (not fields or variant names) defined in both `Typedtree` and
  `Types` begin with the prefix `type_foo`

  There is a lot of redundancy between the two trees.

  Typedtree Module:                                Types Module:
  ----------------------                           ----------------------
  type_declaration = {                          -->type type_declaration = {
                                               /     type_params: type_expr list,
    typ_id: Ident.t,                          /      type_arity: int,
    typ_name: string loc,                    /       type_kind: type_kind,
    typ_params: (core_type * variance) list,/        type_private: private_flag,
    typ_type: Types.type_declaration,  ----/         type_manifest: type_expr option,
    typ_cstrs:(core_type*core_type*Location.t)list   type_variance: Variance.t list,
    typ_kind: type_kind,                             (* covariant, contravariant, weakly contravariant, injective *)
    typ_private: private_flag,                       type_is_newtype: bool,
    typ_manifest: core_type option,                  type_expansion_scope: int option,
    typ_loc: Location.t,                             type_loc: Location.t,
    typ_attributes: attributes                       type_attributes: Parsetree.attributes,
    typ_attributes: attributes                       type_immediate: bool, (* true iff type should not be a pointer *)
  }                                                  type_unboxed: unboxed_status,
                                                   }

  Typedtree Module:                                Types Module:
  ----------------------                           ----------------------
  type type_kind =                                 type type_kind =
  | Ttype_abstract                                 | Type_abstract
  | Ttype_variant of constructor_declaration list  | Type_record of label_declaration list * record_representation
  | Ttype_record of label_declaration list         | Type_variant of constructor_declaration list
  | Ttype_open                                     | Type_open

  Typedtree Module:                                Types Module:
  ----------------------                           ----------------------
  type constructor_declaration = {                 type constructor_declaration = {
    cd_id: Ident.t,                                   cd_id: Ident.t,
    cd_name: string loc,                              cd_args: constructor_arguments,
    cd_args: constructor_arguments,                   cd_res: type_expr option,
    cd_res: core_type option,                         cd_loc: Location.t,
    cd_loc: Location.t,                               cd_attributes: Parsetree.attributes,
    cd_attributes: attributes                      }
  }

  type constructor_arguments =                     type constructor_arguments =
  | Cstr_tuple of core_type list                    | Cstr_tuple of type_expr list
  | Cstr_record of label_declaration list           | Cstr_record of label_declaration list


                        This pointer is mutated
  Typedtree Module:    with the result of type     Types Module:
  -------------        checking once complete!     -------------
  type core_type = {                          ---> type type_expr = {
    mutable ctyp_desc : core_type_desc;      /       mutable desc: type_desc;
    mutable ctyp_type : type_expr; ---------/        mutable level: int;
    ctyp_env : Env.t;                                mutable scope: int option;
    ctyp_loc : Location.t;                           id: int
    ctyp_attributes: attributes;                   }
  }

  type core_type_desc =                            type_desc =
  | Ttyp_any                                        | Tvar of string option
  | Ttyp_var of string                              | Tarrow of arg_label * type_expr * type_expr * commutable
  | Ttyp_arrow of arg_label*core_type*core_type     | Ttuple of type_expr list
  | Ttyp_tuple of core_type list                    | Tconstr of Path.t * type_expr list * abbrev_memo ref
  | Ttyp_constr of                                  | Tobject of type_expr * (Path.t * type_expr list) option ref
    Path.t*Longident.t loc*core_type list           | Tfield of string * field_kind * type_expr * type_expr
  | Ttyp_object of object_field list * closed_flag  | Tnil
  | Ttyp_class of                                   | Tlink of type_expr
     Path.t * Longident.t loc * core_type list      | Tsubst of type_expr         (* for copying *)
  | Ttyp_alias of core_type * string                | Tvariant of row_desc
  | Ttyp_variant of                                 | Tunivar of string option
     row_field list*closed_flag*label list option   | Tpoly of type_expr * type_expr list
  | Ttyp_poly of string list * core_type
  | Ttyp_package of package_type

  Typedtree.type_declaration will be something like: {
    type_type: {
      type_params: ...
      type_kind: type_kind =
         | Type_abstract
         | Type_record of label_declaration list  * record_representation
         | Type_variant of constructor_declaration list
         | Type_open
      type_manifest: Some({
        mutable desc: type_desc =
           | Tvar of string option
           | Tarrow of arg_label * type_expr * type_expr * commutable
           | Ttuple of type_expr list
           | Tconstr of Path.t * type_expr list * abbrev_memo ref
           | Tobject of type_expr * (Path.t * type_expr list) option ref
           | Tfield of string * field_kind * type_expr * type_expr
           | Tnil
           | Tlink of type_expr
           | Tsubst of type_expr         (* for copying *)
           | Tvariant of row_desc
           | Tunivar of string option
           | Tpoly of type_expr * type_expr list
      }),
      type_arity: int;
      type_private: private_flag;
      type_variance: Variance.t list;
      type_is_newtype: bool;
      type_expansion_scope: int option;
      type_loc: Location.t;
      type_attributes: Parsetree.attributes;
      type_immediate: bool; (* true iff type should not be a pointer *)
      type_unboxed: unboxed_status;
    }
  }
 */

let rec removeOption = (~label, typeExpr: Types.type_expr) =>
  switch (typeExpr.desc) {
  | Tconstr(Path.Pident(id), [t], _)
      /* This has a different representation in 4.03+ */
      when Ident.name(id) == "option" && label != "" && label.[0] == '?' =>
    Some((String.sub(label, 1, String.length(label) - 1), t))
  | Tconstr(Pdot(Path.Pident(nameSpace), id, _), [t], _)
      /* This has a different representation in 4.03+ */
      when
        Ident.name(nameSpace) == "FB"
        && id == "option"
        && label != ""
        && label.[0] == '?' =>
    Some((String.sub(label, 1, String.length(label) - 1), t))
  | Tlink(t) => t |> removeOption(~label)
  | _ => None
  };

let translateConstr =
    (~path: Path.t, ~paramsTranslation, ~typeEnv, ~fieldsTranslations) =>
  switch (path, paramsTranslation) {
  | (Pdot(Pident({name: "FB", _}), "bool", _), [])
  | (Pident({name: "bool", _}), []) => {dependencies: [], typ: booleanT}

  | (Pdot(Pident({name: "FB", _}), "int", _), [])
  | (Pident({name: "int", _}), []) => {dependencies: [], typ: numberT}

  | (Pdot(Pident({name: "FB", _}), "float", _), [])
  | (Pident({name: "float", _}), []) => {dependencies: [], typ: numberT}

  | (Pdot(Pident({name: "FB", _}), "string", _), [])
  | (Pident({name: "string", _}), []) => {dependencies: [], typ: stringT}

  | (Pdot(Pident({name: "FB", _}), "unit", _), [])
  | (Pident({name: "unit", _}), []) => {dependencies: [], typ: unitT}

  | (Pdot(Pident({name: "FB", _}), "array", _), [paramTranslation])
  | (Pident({name: "array", _}), [paramTranslation])
  | (
      Pdot(Pdot(Pident({name: "Js", _}), "Array", _), "t", _),
      [paramTranslation],
    ) => {
      ...paramTranslation,
      typ: Array(paramTranslation.typ, Mutable),
    }

  | (Pdot(Pident({name: "ImmutableArray", _}), "t", _), [paramTranslation]) => {
      ...paramTranslation,
      typ: Array(paramTranslation.typ, Immutable),
    }

  | (Pdot(Pident({name: "FB", _}), "option", _), [paramTranslation])
  | (Pident({name: "option", _}), [paramTranslation]) => {
      ...paramTranslation,
      typ: Option(paramTranslation.typ),
    }

  | (
      Pdot(Pdot(Pident({name: "Js", _}), "Nullable", _), "t", _),
      [paramTranslation],
    )
  | (Pdot(Pident({name: "Js", _}), "nullable", _), [paramTranslation])
  | (
      Pdot(Pdot(Pident({name: "Js", _}), "Null_undefined", _), "t", _),
      [paramTranslation],
    )
  | (
      Pdot(Pident({name: "Js", _}), "null_undefined", _),
      [paramTranslation],
    ) => {
      ...paramTranslation,
      typ: Nullable(paramTranslation.typ),
    }
  | (
      Pdot(Pdot(Pident({name: "Js", _}), "Internal", _), "fn", _),
      [{dependencies: argsDependencies, typ: Tuple(ts)}, ret],
    ) => {
      dependencies: argsDependencies @ ret.dependencies,
      typ: Function({typeVars: [], argTypes: ts, retType: ret.typ}),
    }
  | (
      Pdot(Pdot(Pident({name: "Js", _}), "Internal", _), "fn", _),
      [
        {
          dependencies: argsDependencies,
          typ: Enum({cases: [{label: "Arity_0", _}]}),
        },
        ret,
      ],
    ) => {
      dependencies: argsDependencies @ ret.dependencies,
      typ: Function({typeVars: [], argTypes: [], retType: ret.typ}),
    }
  | (
      Pdot(Pdot(Pident({name: "Js", _}), "Internal", _), "fn", _),
      [{dependencies: argsDependencies, typ: singleT}, ret],
    ) => {
      dependencies: argsDependencies @ ret.dependencies,
      typ: Function({typeVars: [], argTypes: [singleT], retType: ret.typ}),
    }
  | (Pdot(Pident({name: "Js", _}), "t", _), _) =>
    let dependencies =
      fieldsTranslations
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
      fieldsTranslations
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

  | _ =>
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
  };

type processVariant = {
  noPayloads: list(string),
  payloads: list((string, Types.type_expr)),
  unknowns: list(string),
};

let processVariant = rowFields => {
  let rec loop = (~noPayloads, ~payloads, ~unknowns, fields) =>
    switch (fields) {
    | [
        (
          label,
          Types.Rpresent(/* no payload */ None) |
          Reither(/* constant constructor */ true, _, _, _),
        ),
        ...otherFields,
      ] =>
      otherFields
      |> loop(~noPayloads=[label, ...noPayloads], ~payloads, ~unknowns)
    | [(label, Rpresent(Some(payload))), ...otherFields] =>
      otherFields
      |> loop(
           ~noPayloads,
           ~payloads=[(label, payload), ...payloads],
           ~unknowns,
         )
    | [(label, Rabsent | Reither(false, _, _, _)), ...otherFields] =>
      otherFields
      |> loop(~noPayloads, ~payloads, ~unknowns=[label, ...unknowns])
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
          typeExpr: Types.type_expr,
        ) =>
  switch (typeExpr.desc) {
  | Tlink(t) =>
    translateArrowType(
      ~config,
      ~typeVarsGen,
      ~noFunctionReturnDependencies,
      ~typeEnv,
      ~revArgDeps,
      ~revArgs,
      t,
    )
  | Tarrow("", typExpr1, typExpr2, _) =>
    let {dependencies, typ} =
      typExpr1
      |> translateTypeExprFromTypes_(~config, ~typeVarsGen, ~typeEnv, _);
    let nextRevDeps = List.rev_append(dependencies, revArgDeps);
    typExpr2
    |> translateArrowType(
         ~config,
         ~typeVarsGen,
         ~noFunctionReturnDependencies,
         ~typeEnv,
         ~revArgDeps=nextRevDeps,
         ~revArgs=[(Nolabel, typ), ...revArgs],
       );
  | Tarrow(label, typExpr1, typExpr2, _) =>
    switch (typExpr1 |> removeOption(~label)) {
    | None =>
      let {dependencies, typ: typ1} =
        typExpr1
        |> translateTypeExprFromTypes_(~config, ~typeVarsGen, ~typeEnv);
      let nextRevDeps = List.rev_append(dependencies, revArgDeps);
      typExpr2
      |> translateArrowType(
           ~config,
           ~typeVarsGen,
           ~noFunctionReturnDependencies,
           ~typeEnv,
           ~revArgDeps=nextRevDeps,
           ~revArgs=[(Label(label), typ1), ...revArgs],
         );
    | Some((lbl, t1)) =>
      let {dependencies, typ: typ1} =
        t1 |> translateTypeExprFromTypes_(~config, ~typeVarsGen, ~typeEnv);
      let nextRevDeps = List.rev_append(dependencies, revArgDeps);
      typExpr2
      |> translateArrowType(
           ~config,
           ~typeVarsGen,
           ~noFunctionReturnDependencies,
           ~typeEnv,
           ~revArgDeps=nextRevDeps,
           ~revArgs=[(OptLabel(lbl), typ1), ...revArgs],
         );
    }
  | _ =>
    let {dependencies, typ: retType} =
      typeExpr |> translateTypeExprFromTypes_(~config, ~typeVarsGen, ~typeEnv);
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
and translateTypeExprFromTypes_ =
    (
      ~config,
      ~typeVarsGen,
      ~noFunctionReturnDependencies=false,
      ~typeEnv,
      typeExpr: Types.type_expr,
    ) =>
  switch (typeExpr.desc) {
  | Tvar(None) =>
    let typeName =
      GenIdent.jsTypeNameForAnonymousTypeID(~typeVarsGen, typeExpr.id);
    {dependencies: [], typ: TypeVar(typeName)};

  | Tvar(Some(s)) => {dependencies: [], typ: TypeVar(s)}

  | Tconstr(
      Pdot(Pident({name: "Js", _}), "t", _) as path,
      [{desc: Tobject(tObj, _), _}],
      _,
    ) =>
    let rec getFieldTypes = (texp: Types.type_expr) =>
      switch (texp.desc) {
      | Tfield(name, _, t1, t2) => [
          (
            name,
            name |> Runtime.isMutableObjectField ?
              {dependencies: [], typ: Ident("", [])} :
              t1
              |> translateTypeExprFromTypes_(~config, ~typeVarsGen, ~typeEnv),
          ),
          ...t2 |> getFieldTypes,
        ]
      | _ => []
      };
    let fieldsTranslations = tObj |> getFieldTypes;
    translateConstr(
      ~path,
      ~paramsTranslation=[],
      ~typeEnv,
      ~fieldsTranslations,
    );

  | Tconstr(path, typeParams, _) =>
    let paramsTranslation =
      typeParams
      |> translateTypeExprsFromTypes_(~config, ~typeVarsGen, ~typeEnv);
    translateConstr(
      ~path,
      ~paramsTranslation,
      ~typeEnv,
      ~fieldsTranslations=[],
    );

  | Tpoly(t, []) =>
    t
    |> translateTypeExprFromTypes_(
         ~config,
         ~typeVarsGen,
         ~noFunctionReturnDependencies,
         ~typeEnv,
       )

  | Tarrow(_) =>
    typeExpr
    |> translateArrowType(
         ~config,
         ~typeVarsGen,
         ~noFunctionReturnDependencies,
         ~typeEnv,
         ~revArgDeps=[],
         ~revArgs=[],
       )
  | Ttuple(listExp) =>
    let innerTypesTranslation =
      listExp |> translateTypeExprsFromTypes_(~config, ~typeVarsGen, ~typeEnv);
    let innerTypes = innerTypesTranslation |> List.map(({typ, _}) => typ);
    let innerTypesDeps =
      innerTypesTranslation
      |> List.map(({dependencies, _}) => dependencies)
      |> List.concat;

    let tupleType = Tuple(innerTypes);

    {dependencies: innerTypesDeps, typ: tupleType};

  | Tlink(t) =>
    t
    |> translateTypeExprFromTypes_(
         ~config,
         ~typeVarsGen,
         ~noFunctionReturnDependencies,
         ~typeEnv,
       )

  | Tvariant(rowDesc) =>
    switch (rowDesc.row_fields |> processVariant) {
    | {noPayloads, payloads: [], unknowns: []} =>
      let cases =
        noPayloads |> List.map(label => {label, labelJS: StringLabel(label)});
      let typ = cases |> createEnum(~withPayload=[], ~polyVariant=false);
      {dependencies: [], typ};

    | {noPayloads: [], payloads: [(_label, t)], unknowns: []} =>
      /* Handle bucklescript's "Arity_" encoding in first argument of Js.Internal.fn(_,_) for uncurried functions.
         Return the argument tuple. */
      t
      |> translateTypeExprFromTypes_(
           ~config,
           ~typeVarsGen,
           ~noFunctionReturnDependencies,
           ~typeEnv,
         )

    | {noPayloads, payloads, unknowns: []} =>
      let cases =
        noPayloads |> List.map(label => {label, labelJS: StringLabel(label)});
      let payloadTranslations =
        payloads
        |> List.map(((label, payload)) =>
             (
               label,
               payload
               |> translateTypeExprFromTypes_(~config, ~typeVarsGen, ~typeEnv),
             )
           );
      let withPayload =
        payloadTranslations
        |> List.map(((label, translation)) =>
             ({label, labelJS: StringLabel(label)}, translation.typ)
           );
      let typ = cases |> createEnum(~withPayload, ~polyVariant=true);
      let dependencies =
        payloadTranslations
        |> List.map(((_, {dependencies, _})) => dependencies)
        |> List.concat;
      {dependencies, typ};

    | {unknowns: [_, ..._]} => {
        dependencies: [],
        typ: mixedOrUnknown(~config),
      }
    }

  | Tpackage(path, _ids, _typs) =>
    let rec signatureToRecordType = signature => {
      let dependenciesAndFields =
        signature
        |> List.map(signatureItem =>
             switch (signatureItem) {
             | Types.Sig_value(_id, {val_kind: Val_prim(_), _}) => ([], [])
             | Types.Sig_value(id, {val_type: typeExpr, _}) =>
               let {dependencies, typ} =
                 typeExpr
                 |> translateTypeExprFromTypes_(
                      ~config,
                      ~typeVarsGen,
                      ~noFunctionReturnDependencies=false,
                      ~typeEnv,
                    );
               let field = {
                 name: id |> Ident.name,
                 optional: Mandatory,
                 mutable_: Immutable,
                 typ,
               };
               (dependencies, [field]);

             | Types.Sig_module(id, moduleDeclaration, _recStatus) =>
               let (dependencies, typ) =
                 switch (moduleDeclaration.md_type) {
                 | Mty_signature(signature) =>
                   signature |> signatureToRecordType
                 | Mty_ident(_)
                 | Mty_functor(_)
                 | Mty_alias(_) => ([], mixedOrUnknown(~config))
                 };
               let field = {
                 name: id |> Ident.name,
                 optional: Mandatory,
                 mutable_: Immutable,
                 typ,
               };
               (dependencies, [field]);

             | Types.Sig_type(_)
             | Types.Sig_typext(_)
             | Types.Sig_modtype(_)
             | Types.Sig_class(_)
             | Types.Sig_class_type(_) => ([], [])
             }
           );
      let (dependencies, fields) = {
        let (dl, fl) = dependenciesAndFields |> List.split;
        (dl |> List.concat, fl |> List.concat);
      };
      (dependencies, Record(fields));
    };

    switch (typeEnv |> TypeEnv.lookupModuleTypeSignature(~path)) {
    | Some(signature) =>
      let (dependencies, typ) = signature.sig_type |> signatureToRecordType;
      {dependencies, typ};
    | None => {dependencies: [], typ: mixedOrUnknown(~config)}
    };

  | Tfield(_)
  | Tnil
  | Tobject(_)
  | Tpoly(_)
  | Tsubst(_)
  | Tunivar(_) => {dependencies: [], typ: mixedOrUnknown(~config)}
  }
and translateTypeExprsFromTypes_ =
    (~config, ~typeVarsGen, ~typeEnv, typeExprs): list(translation) =>
  typeExprs
  |> List.map(translateTypeExprFromTypes_(~config, ~typeVarsGen, ~typeEnv));

let translateTypeExprFromTypes =
    (~config, ~noFunctionReturnDependencies=?, ~typeEnv, typeExpr) => {
  let typeVarsGen = GenIdent.createTypeVarsGen();
  let translation =
    typeExpr
    |> translateTypeExprFromTypes_(
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

let translateTypeExprsFromTypes = (~config, ~typeEnv, typeExprs) => {
  let typeVarsGen = GenIdent.createTypeVarsGen();
  let translations =
    typeExprs |> translateTypeExprsFromTypes_(~config, ~typeVarsGen, ~typeEnv);

  if (Debug.dependencies^) {
    translations
    |> List.iter(translation =>
         translation.dependencies
         |> List.iter(dep =>
              logItem("Dependency: %s\n", dep |> Dependencies.typePathToName)
            )
       );
  };
  translations;
};