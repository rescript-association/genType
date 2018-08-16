open GenFlowCommon;

type t =
  /* Import a type that we expect to also be genFlow'd. */
  | TypeAtPath(Path.t)
  | FreeTypeVariable(string);

type conversionPlan = {
  dependencies: list(t),
  convertableType,
};

let rec typePathToName = typePath =>
  switch (typePath) {
  | Path.Pident(id) => Ident.name(id)
  | Pdot(p, s, _pos) => typePathToName(p) ++ s
  | Papply(p1, p2) =>
    typePathToName(p1) ++ "__unsupported_genFlow__" ++ typePathToName(p2)
  };

let toString = dep =>
  switch (dep) {
  | TypeAtPath(path) => "TypeAtPath(" ++ typePathToName(path) ++ ")"
  | FreeTypeVariable(s) => "FreeTypeVariable(" ++ s ++ ")"
  };

let hasTypeVar = dep =>
  switch (dep) {
  | FreeTypeVariable(_) => true
  | TypeAtPath(_) => false
  };

let extractFreeTypeVars = deps =>
  if (deps |> List.exists(hasTypeVar)) {
    let (revFreeTypeVars, revRemainingDeps) =
      List.fold_left(
        ((revFreeTypeVars, revRemainingDeps) as soFar, nextDep) =>
          switch (nextDep) {
          | FreeTypeVariable(s) =>
            revFreeTypeVars |> List.exists(s2 => s2 == s) ?
              soFar : ([s, ...revFreeTypeVars], revRemainingDeps)
          | _ => (revFreeTypeVars, [nextDep, ...revRemainingDeps])
          },
        ([], []),
        deps,
      );
    (revFreeTypeVars |> List.rev, revRemainingDeps |> List.rev);
  } else {
    ([], deps);
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

let rec removeOption = (label, type_expr: Types.type_expr) =>
  switch (type_expr.desc) {
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

let rec extract_fun_ =
        (
          ~language,
          ~typeVarsGen,
          ~noFunctionReturnDependencies=false,
          revArgDeps,
          revArgs,
          type_expr: Types.type_expr,
        ) =>
  switch (type_expr.desc) {
  | Tlink(t) =>
    extract_fun_(
      ~language,
      ~typeVarsGen,
      ~noFunctionReturnDependencies,
      revArgDeps,
      revArgs,
      t,
    )
  | Tarrow("", t1, t2, _) =>
    let {dependencies, convertableType} =
      typeExprToConversion_(~language, ~typeVarsGen, t1);
    let nextRevDeps = List.rev_append(dependencies, revArgDeps);
    extract_fun_(
      ~language,
      ~typeVarsGen,
      ~noFunctionReturnDependencies,
      nextRevDeps,
      [(Nolabel, convertableType), ...revArgs],
      t2,
    );
  | Tarrow(lbl, t1, t2, _) =>
    switch (removeOption(lbl, t1)) {
    | None =>
      /* TODO: Convert name to object, convert null to optional. */
      let {dependencies, convertableType: t1Conversion} =
        typeExprToConversion_(~language, ~typeVarsGen, t1);
      let nextRevDeps = List.rev_append(dependencies, revArgDeps);
      extract_fun_(
        ~language,
        ~typeVarsGen,
        ~noFunctionReturnDependencies,
        nextRevDeps,
        [(Label(lbl), t1Conversion), ...revArgs],
        t2,
      );
    | Some((lbl, t1)) =>
      let {dependencies, convertableType: (t1Converter, t1Typ)} =
        typeExprToConversion_(~language, ~typeVarsGen, t1);
      let t1Conversion = (OptionalArgument(t1Converter), t1Typ);
      let nextRevDeps = List.rev_append(dependencies, revArgDeps);
      /* TODO: Convert name to object, convert null to optional. */
      extract_fun_(
        ~language,
        ~typeVarsGen,
        ~noFunctionReturnDependencies,
        nextRevDeps,
        [(OptLabel(lbl), t1Conversion), ...revArgs],
        t2,
      );
    }
  | _ =>
    let {dependencies, convertableType: (retConverter, retType)} =
      typeExprToConversion_(~language, ~typeVarsGen, type_expr);
    let allDeps =
      List.rev_append(
        revArgDeps,
        noFunctionReturnDependencies ? [] : dependencies,
      );

    let labeledConvertableTypes = revArgs |> List.rev;
    let groupedArgs = labeledConvertableTypes |> NamedArgs.group;

    let groupedArgToConverter = groupedArg =>
      switch (groupedArg) {
      | Group(group) =>
        GroupConverter(
          group |> List.map(((s, _optionalness, (c, _t))) => (s, c)),
        )
      | Arg((c, _t)) => ArgConverter(Nolabel, c)
      };
    let groupedArgConverters = groupedArgs |> List.map(groupedArgToConverter);
    /* TODO: Ignore all final single unit args at convert/type conversion time. */
    let notJustASingleUnitArg =
      switch (labeledConvertableTypes) {
      | [(Nolabel, (c, _t))] when c === Unit => false
      | _ => true
      };
    let needsArgConversion =
      List.exists(
        ((lbl, (converter, _t))) =>
          lbl !== Nolabel || converter !== Identity,
        labeledConvertableTypes,
      )
      && notJustASingleUnitArg;
    let functionConverter =
      retConverter !== Identity || needsArgConversion ?
        Fn((groupedArgConverters, retConverter)) : Identity;

    let groupedArgToTyp = groupedArg =>
      switch (groupedArg) {
      | Group(group) =>
        let fields =
          group
          |> List.map(((s, optionalness, (_c, typ))) =>
               (s, optionalness, typ)
             );
        ObjectType(fields);
      | Arg((_converter, typ)) => typ
      };
    let functionType =
      Arrow([], groupedArgs |> List.map(groupedArgToTyp), retType);

    {
      dependencies: allDeps,
      convertableType: (functionConverter, functionType),
    };
  }
/**
 * Convertes Types.type_expr to:
 *
 *   (list(dependency), option(expressionConverter), renderedType)).
 *
 * - `list(dependency)`: The Reason types types used in the type structure.  It
 * must be ensured that these are then imported.
 * - `option(expressionConverter)`: Any converter that is required for this type.
 * - `renderedType` the flow type (just a string prepresentation)
 * TODO: Handle the case where the function in Reason accepts a single unit
 * arg, which should NOT be converted.
 */
and typeExprToConversion_ =
    (
      ~language,
      ~typeVarsGen,
      ~noFunctionReturnDependencies=false,
      type_expr: Types.type_expr,
    )
    : conversionPlan =>
  switch (type_expr.desc) {
  | Tvar(None) =>
    let typeName =
      GenIdent.jsTypeNameForAnonymousTypeID(~typeVarsGen, type_expr.id);
    {
      dependencies: [FreeTypeVariable(typeName)],
      convertableType: (Identity, Ident(typeName, [])),
    };
  | Tvar(Some(s)) =>
    let typeName = s;
    {
      dependencies: [FreeTypeVariable(typeName)],
      convertableType: (Identity, Ident(s, [])),
    };
  | Tconstr(Pdot(Path.Pident({Ident.name: "FB", _}), "bool", _), [], _)
  | Tconstr(Path.Pident({name: "bool", _}), [], _) => {
      dependencies: [],
      convertableType: (Identity, Ident("boolean", [])),
    }
  | Tconstr(Pdot(Path.Pident({Ident.name: "FB", _}), "int", _), [], _)
  | Tconstr(Path.Pident({name: "int", _}), [], _) => {
      dependencies: [],
      convertableType: (Identity, Ident("number", [])),
    }
  | Tconstr(Pdot(Path.Pident({Ident.name: "FB", _}), "string", _), [], _)
  | Tconstr(Path.Pident({name: "string", _}), [], _) => {
      dependencies: [],
      convertableType: (Identity, Ident("string", [])),
    }
  | Tconstr(Pdot(Path.Pident({Ident.name: "FB", _}), "unit", _), [], _)
  | Tconstr(Path.Pident({name: "unit", _}), [], _) => {
      dependencies: [],
      convertableType: (Unit, Ident("(typeof undefined)", [])),
    }
  /*
   * Arrays do not experience any conversion, in order to retain referencial
   * equality. This poses a problem for Arrays that contain option types
   * which require conversion. The solution here could be to use the Reason
   * representation of option for everything except named arguments.
   */
  | Tconstr(Pdot(Path.Pident({Ident.name: "FB", _}), "array", _), [p], _)
  | Tconstr(Path.Pident({name: "array", _}), [p], _) =>
    let {dependencies: paramDeps, convertableType: (itemConverter, itemFlow)} =
      typeExprToConversion_(~language, ~typeVarsGen, p);
    if (itemConverter === Identity) {
      {
        dependencies: paramDeps,
        convertableType: (Identity, Ident("$ReadOnlyArray", [itemFlow])),
      };
    } else {
      raise(
        Invalid_argument(
          "Converting Arrays with elements that require conversion "
          ++ "is not yet supported. Saw an array containing type:"
          ++ EmitTyp.typToString(~language, itemFlow),
        ),
      );
    };
  | Tconstr(Pdot(Path.Pident({Ident.name: "FB", _}), "option", _), [p], _)
  | Tconstr(Path.Pident({name: "option", _}), [p], _) =>
    /* TODO: Handle / verify the case of nested optionals. */
    let {
      dependencies: paramDeps,
      convertableType: (paramConverter, paramConverted),
    } =
      typeExprToConversion_(~language, ~typeVarsGen, p);
    let composedConverter = Option(paramConverter);
    {
      dependencies: paramDeps,
      convertableType: (composedConverter, Optional(paramConverted)),
    };
  | Tarrow(_) =>
    extract_fun_(
      ~language,
      ~typeVarsGen,
      ~noFunctionReturnDependencies,
      [],
      [],
      type_expr,
    )
  | Tlink(t) =>
    typeExprToConversion_(
      ~language,
      ~typeVarsGen,
      ~noFunctionReturnDependencies,
      t,
    )
  | Tconstr(path, [], _) => {
      dependencies: [TypeAtPath(path)],
      convertableType: (Identity, Ident(typePathToName(path), [])),
    }
  /* This type doesn't have any built in converter. But what if it was a
   * genFlow variant type? */
  /*
   * Built-in standard library parameterized types (aside from option) are
   * like custom parameterized types in that they don't undergo conversion,
   * and their type parameter's dependencies are tracked.  For example
   * `list(int)` will be treated just like a custom type named List that.
   * There is special treatment of TypeAtPath("list") to make sure the
   * built-in JS type defs are brought in from the right location.
   */
  | Tconstr(path, typeParams, _) =>
    let conversionPlans =
      typeExprsToConversion_(~language, ~typeVarsGen, typeParams);
    let convertableTypes =
      conversionPlans |> List.map(({convertableType, _}) => convertableType);
    let typeParamDeps =
      conversionPlans
      |> List.map(({dependencies, _}) => dependencies)
      |> List.concat;
    /* How is this exprConv completely ignored? */
    let typeArgs =
      List.map(((_exprConv, flowTyp: typ)) => flowTyp, convertableTypes);
    {
      dependencies: [TypeAtPath(path), ...typeParamDeps],
      convertableType: (Identity, Ident(typePathToName(path), typeArgs)),
    };
  | _ => {dependencies: [], convertableType: (Identity, any)}
  }
and typeExprsToConversion_ =
    (~language, ~typeVarsGen, typeExprs): list(conversionPlan) =>
  typeExprs |> List.map(typeExprToConversion_(~language, ~typeVarsGen));

let typeExprToConversion =
    (~language, ~noFunctionReturnDependencies=?, typeExpr) => {
  let typeVarsGen = GenIdent.createTypeVarsGen();
  let conversionPlan =
    typeExpr
    |> typeExprToConversion_(
         ~language,
         ~typeVarsGen,
         ~noFunctionReturnDependencies?,
       );
  if (Debug.dependencies) {
    conversionPlan.dependencies
    |> List.iter(dep => logItem("Dependency: %s\n", dep |> toString));
  };
  conversionPlan;
};
let typeExprsToConversion = (~language, typeExprs) => {
  let typeVarsGen = GenIdent.createTypeVarsGen();
  let conversionPlans =
    typeExprs |> typeExprsToConversion_(~language, ~typeVarsGen);
  if (Debug.dependencies) {
    conversionPlans
    |> List.iter(conversionPlan =>
         conversionPlan.dependencies
         |> List.iter(dep => logItem("Dependency: %s\n", dep |> toString))
       );
  };
  conversionPlans;
};