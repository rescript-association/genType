open GenTypeCommon;

type t =
  | TypeAtPath(Path.t);

type translation = {
  dependencies: list(t),
  typ,
};

let rec typePathToName = typePath =>
  switch (typePath) {
  | Path.Pident(id) => Ident.name(id)
  | Pdot(p, s, _pos) => typePathToName(p) ++ "_" ++ s
  | Papply(p1, p2) =>
    typePathToName(p1) ++ "__unsupported_genType__" ++ typePathToName(p2)
  };

let toString = dep =>
  switch (dep) {
  | TypeAtPath(path) => "TypeAtPath(" ++ typePathToName(path) ++ ")"
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

let rec removeOption = (label, typeExpr: Types.type_expr) =>
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
  | Tlink(t) => removeOption(label, t)
  | _ => None
  };

let rec extract_fun =
        (
          ~language,
          ~typeVarsGen,
          ~noFunctionReturnDependencies=false,
          revArgDeps,
          revArgs,
          typeExpr: Types.type_expr,
        ) =>
  switch (typeExpr.desc) {
  | Tlink(t) =>
    extract_fun(
      ~language,
      ~typeVarsGen,
      ~noFunctionReturnDependencies,
      revArgDeps,
      revArgs,
      t,
    )
  | Tarrow("", typExpr1, typExpr2, _) =>
    let {dependencies, typ} =
      typExpr1 |> translateTypeExpr_(~language, ~typeVarsGen);
    let nextRevDeps = List.rev_append(dependencies, revArgDeps);
    extract_fun(
      ~language,
      ~typeVarsGen,
      ~noFunctionReturnDependencies,
      nextRevDeps,
      [(Nolabel, typ), ...revArgs],
      typExpr2,
    );
  | Tarrow(lbl, typExpr1, typExpr2, _) =>
    switch (removeOption(lbl, typExpr1)) {
    | None =>
      /* TODO: Convert name to object, convert null to optional. */
      let {dependencies, typ: typ1} =
        typExpr1 |> translateTypeExpr_(~language, ~typeVarsGen);
      let nextRevDeps = List.rev_append(dependencies, revArgDeps);
      typExpr2
      |> extract_fun(
           ~language,
           ~typeVarsGen,
           ~noFunctionReturnDependencies,
           nextRevDeps,
           [(Label(lbl), typ1), ...revArgs],
         );
    | Some((lbl, t1)) =>
      let {dependencies, typ: typ1} =
        t1 |> translateTypeExpr_(~language, ~typeVarsGen);
      let nextRevDeps = List.rev_append(dependencies, revArgDeps);
      /* TODO: Convert name to object, convert null to optional. */
      extract_fun(
        ~language,
        ~typeVarsGen,
        ~noFunctionReturnDependencies,
        nextRevDeps,
        [(OptLabel(lbl), typ1), ...revArgs],
        typExpr2,
      );
    }
  | _ =>
    let {dependencies, typ: retType} =
      typeExpr |> translateTypeExpr_(~language, ~typeVarsGen);
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
and translateTypeExpr_ =
    (
      ~language,
      ~typeVarsGen,
      ~noFunctionReturnDependencies=false,
      typeExpr: Types.type_expr,
    )
    : translation =>
  switch (typeExpr.desc) {
  | Tvar(None) =>
    let typeName =
      GenIdent.jsTypeNameForAnonymousTypeID(~typeVarsGen, typeExpr.id);
    {dependencies: [], typ: TypeVar(typeName)};
  | Tvar(Some(s)) => {dependencies: [], typ: TypeVar(s)}
  | Tconstr(Pdot(Pident({name: "FB", _}), "bool", _), [], _)
  | Tconstr(Pident({name: "bool", _}), [], _) => {
      dependencies: [],
      typ: booleanT,
    }
  | Tconstr(Pdot(Pident({name: "FB", _}), "int", _), [], _)
  | Tconstr(Pident({name: "int", _}), [], _) => {
      dependencies: [],
      typ: numberT,
    }
  | Tconstr(Pdot(Pident({name: "FB", _}), "string", _), [], _)
  | Tconstr(Pident({name: "string", _}), [], _) => {
      dependencies: [],
      typ: stringT,
    }
  | Tconstr(Pdot(Pident({name: "FB", _}), "unit", _), [], _)
  | Tconstr(Pident({name: "unit", _}), [], _) => {
      dependencies: [],
      typ: unitT,
    }
  /*
   * Arrays do not experience any conversion, in order to retain referencial
   * equality. This poses a problem for Arrays that contain option types
   * which require conversion. The solution here could be to use the Reason
   * representation of option for everything except named arguments.
   */
  | Tconstr(Pdot(Pident({name: "FB", _}), "array", _), [param], _)
  | Tconstr(Pident({name: "array", _}), [param], _) =>
    let paramTranslation =
      param |> translateTypeExpr_(~language, ~typeVarsGen);
    {...paramTranslation, typ: Array(paramTranslation.typ)};
  | Tconstr(Pdot(Pident({name: "FB", _}), "option", _), [param], _)
  | Tconstr(Pident({name: "option", _}), [param], _) =>
    let paramTranslation =
      param |> translateTypeExpr_(~language, ~typeVarsGen);
    {...paramTranslation, typ: Option(paramTranslation.typ)};
  | Tconstr(
      Pdot(Pdot(Pident({name: "Js", _}), "Nullable", _), "t", _),
      [param],
      _,
    ) =>
    let paramTranslation =
      param |> translateTypeExpr_(~language, ~typeVarsGen);
    {...paramTranslation, typ: Nullable(paramTranslation.typ)};
  | Tarrow(_) =>
    typeExpr
    |> extract_fun(
         ~language,
         ~typeVarsGen,
         ~noFunctionReturnDependencies,
         [],
         [],
       )
  | Tlink(t) =>
    t
    |> translateTypeExpr_(
         ~language,
         ~typeVarsGen,
         ~noFunctionReturnDependencies,
       )
  | Tconstr(path, [], _) => {
      dependencies: [TypeAtPath(path)],
      typ: Ident(typePathToName(path), []),
    }
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
  | Tconstr(path, typeParams, _) =>
    let paramsTranslation =
      typeParams |> translateTypeExprs_(~language, ~typeVarsGen);
    let typeArgs = paramsTranslation |> List.map(({typ, _}) => typ);
    let typeParamDeps =
      paramsTranslation
      |> List.map(({dependencies, _}) => dependencies)
      |> List.concat;
    {
      dependencies: [TypeAtPath(path), ...typeParamDeps],
      typ: Ident(typePathToName(path), typeArgs),
    };
  | _ => {dependencies: [], typ: mixedOrUnknown(~language)}
  }
and translateTypeExprs_ =
    (~language, ~typeVarsGen, typeExprs): list(translation) =>
  typeExprs |> List.map(translateTypeExpr_(~language, ~typeVarsGen));

let translateTypeExpr = (~language, ~noFunctionReturnDependencies=?, typeExpr) => {
  let typeVarsGen = GenIdent.createTypeVarsGen();
  let translation =
    typeExpr
    |> translateTypeExpr_(
         ~language,
         ~typeVarsGen,
         ~noFunctionReturnDependencies?,
       );

  if (Debug.dependencies) {
    translation.dependencies
    |> List.iter(dep => logItem("Dependency: %s\n", dep |> toString));
  };
  translation;
};
let translateTypeExprs = (~language, typeExprs) => {
  let typeVarsGen = GenIdent.createTypeVarsGen();
  let translations =
    typeExprs |> translateTypeExprs_(~language, ~typeVarsGen);

  if (Debug.dependencies) {
    translations
    |> List.iter(translation =>
         translation.dependencies
         |> List.iter(dep => logItem("Dependency: %s\n", dep |> toString))
       );
  };
  translations;
};