/***
 * Copyright 2004-present Facebook. All Rights Reserved.
 */

open Migrate_parsetree;

module ReasonAst = OCaml_404.Ast;

module StringMap = Map.Make(String);
module StringSet = Set.Make(String);

open GenFlowCommon;

/*
 * TODO:
 * Change the name of types/variables "conversion". It is not a conversion, it
 * representation a conversion task.
 */
/*
 * TODO: Inject a Flow opaque type for this variant type. Also inject a .match
 * export binding for the type.
 */

let rec removeOption = (label, typ) =>
  Types.(
    switch (typ.desc) {
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
    }
  );

let rec hasAttribute = (searchText, attributes) =>
  switch (attributes) {
  | [] => false
  | [({Asttypes.txt}, _), ...tl] when txt == searchText => true
  | [hd, ...tl] => hasAttribute(searchText, tl)
  };

/*
 * A place to put all the customization of the generated output for various
 * backends such as:
 * - Alternative Typed Language: Flow, TS?
 * - Module Systems: CommonJS/providesModule.
 * - Binding Strategy: Injecting raw JS (bs.raw) vs. using extern API.
 *
 * Only supporting Flow + providesModule + using bs.raw for now.
 */
module Generator = {
  /**
   * Returns the generated JS module name for a given Reason module name.
   */
  let jsModuleNameForReasonModuleName = (modulesMap, reasonModuleName) => {
    let tentative = reasonModuleName ++ ".bs";
    StringMap.mem(tentative, modulesMap) ?
      StringMap.find(tentative, modulesMap) : tentative;
  };
  /**
   * Returns the *output* Reason module name for an input Reason module name.
   * Note that this is not the JS/providesModule module name.
   */
  let outputReasonModuleName = inputReasonModuleName =>
    inputReasonModuleName ++ "Flow";
  let tagSearch = "genFlow";
  let tagSearchOpaque = "genFlow.opaque";
  let componentTagSearch = tagSearch;
  let jsTypeNameForAnonymousTypeID = id => "T" ++ string_of_int(id);
  let jsTypeNameForTypeParameterName = s => String.capitalize;
};

type genFlowKind =
  | NoGenFlow
  | GenFlow
  | GenFlowOpaque;

let getGenFlowKind = attrs =>
  if (hasAttribute(Generator.tagSearch, attrs)) {
    GenFlow;
  } else if (hasAttribute(Generator.tagSearchOpaque, attrs)) {
    GenFlowOpaque;
  } else {
    NoGenFlow;
  };

/**
 * A pair of functions:
 * - One function that can transform an expression representing JavaScript
 * runtime value into an expression that converts that runtime value into the
 * appropriate Reason runtime value.
 * - The other, performing the opposite AST transformation.
 *
 *     let boolConverter = {
 *       toJS: (exprOfTypeJSBool) => exprThatTransformsThatExprIntoReasonBool,
 *       toReason: (exprOfTypeReasonBool) => exprThatTransformsThatExprIntoJSBool
 *     };
 *     let optionalConverter = {
 *       toJS: (exprOfTypeJSNullableX) => exprThatTransformsThatExprIntoReasonOptional,
 *       toReason: (exprOfTypeReasonOptional) => exprThatTransformsThatExprIntoJSNullable
 *     };
 *
 * So when automatically creating bindings for the Reason function:
 *
 *     let isTrue = (b : option(bool)) : bool =>
 *       switch(b) {
 *       | None => false
 *       | Some(b_) => b_ === true
 *       };
 *
 * You would expose a wrapper of that function to Flow of the form:
 *     let isTrue' = (a) =>
 *       <boolConverter.toJS>(isTrue(
 *         <optionalConverter.toReason>(
 *           <boolConverter.toReason>(a)
 *         )
 *       ))
 *
 * Where the <> regions represent AST-processing substitutions.
 */
type expressionConverter = {
  toJS: ReasonAst.Parsetree.expression => ReasonAst.Parsetree.expression,
  toReason: ReasonAst.Parsetree.expression => ReasonAst.Parsetree.expression,
};

/**
 * Allows conveniently organizing arguments into groupings of named args. This
 * is useful for when mapping those patterns to JS functions that accept
 * objects and turn them into calls into Reason named args.
 */
type jsIdentifier = string;

type groupedArg('t) =
  /* Contains a list of (name, isOptional, 't)  */
  | NamedArgs(list((string, optionalness, 't)))
  | Arg('t);

/**
 * For convenient processing turns consecutive named arguments into a
 * `NamedArgs` group, and individual non-named arguments into `Arg`s.
 */
let rec groupReversed = (revCurGroup, revResult, lst) =>
  switch (revCurGroup, lst) {
  | ([], [(Nolabel, t), ...tl]) =>
    groupReversed([], [Arg(t), ...revResult], tl)
  /* Add it to the current group, not result. */
  | (_, [(OptLabel(name), t), ...tl]) =>
    groupReversed([(name, NonMandatory, t), ...revCurGroup], revResult, tl)
  | (_, [(Label(name), t), ...tl]) =>
    groupReversed([(name, Mandatory, t), ...revCurGroup], revResult, tl)
  | ([], []) => revResult
  | ([grpHd, ...grpTl], [] as _tl)
  /* Just form the group, and recurse ignoring the (None, t) in that case.
   * it will be handled in recursion. */
  | ([grpHd, ...grpTl], [(Nolabel, _), ..._tl]) =>
    groupReversed([], [NamedArgs(revCurGroup), ...revResult], lst)
  };

/**
 * Special reverse that not only reverses the entire list but also the order of
 * items in the NamedArgs grouping.
 */
let rec reverse = (~soFar=[], lst) =>
  switch (lst) {
  | [] => soFar
  | [Arg(t) as hd, ...tl] => reverse(~soFar=[hd, ...soFar], tl)
  | [NamedArgs(namedArgs), ...tl] =>
    reverse(~soFar=[NamedArgs(List.rev(namedArgs)), ...soFar], tl)
  };

/**
 * Converts Reason values of the following form:
 *
 *     (~x: number, ~y: number) => x + y
 *
 * Into JS functions of the following form:
 *
 *     (jsObj) => thatReasonFunction(/*~x=*/jsObj.x, /*~y=*/jsObj.y)
 *
 * Because we start with Reason as the source of truth for libraries, we have
 * all the type information for callbacks.
 *
 * Use case:
 * ---------
 *
 * Module.re
 *
 *     [@genFlow]
 *     let utility = (~x, ~y) => x + y;
 *
 * Consumer.js
 *
 *     const result = Module.utility({x:0, y:100});
 */
let convertFunToJS = (origFun, groupedArgConverters, retConverter) => {
  let rec createFun = (revAppArgsSoFar, groupedArgConverters) =>
    switch (groupedArgConverters) {
    | [] =>
      let return =
        GenFlowEmitAst.mkExprApplyFunLabels(
          origFun,
          List.rev(revAppArgsSoFar),
        );
      retConverter.toJS(return);
    | [Arg(conv), ...tl] =>
      let newIdent = GenIdent.argIdent();
      let nextApp = (
        ReasonAst.Asttypes.Nolabel,
        conv.toReason(GenFlowEmitAst.mkExprIdentifier(newIdent)),
      );
      GenFlowEmitAst.mkExprFun(
        newIdent,
        createFun([nextApp, ...revAppArgsSoFar], tl),
      );
    | [NamedArgs(nameds), ...tl] =>
      let newIdent = GenIdent.argIdent();
      let mapToAppArg = ((name, optness, conv)) => {
        let ident = GenFlowEmitAst.mkExprIdentifier(newIdent);
        switch (optness) {
        | Mandatory => (
            ReasonAst.Asttypes.Labelled(name),
            conv.toReason(GenFlowEmitAst.mkJSGet(ident, name)),
          )
        | NonMandatory => (
            ReasonAst.Asttypes.Optional(name),
            conv.toReason(GenFlowEmitAst.mkJSGet(ident, name)),
          )
        };
      };
      let newRevArgs = List.rev_map(mapToAppArg, nameds);
      let nextRevAppArgs = List.append(newRevArgs, revAppArgsSoFar);
      GenFlowEmitAst.mkExprFun(newIdent, createFun(nextRevAppArgs, tl));
    };
  createFun([], groupedArgConverters);
};

/**
 * Converts JS values such as the following:
 *
 *     (jsObj) => jsObj.x + jsObj.y
 *    /      \
 *    --------
 *   Where we assume that a grouping of Reason arguments has a corresponding
 *   JavaScript object containing the arguments keyed by their named argument
 *   labels.
 *
 * Into Reason functions of the following form:
 *
 *     (~x: number, ~y: number) => (calls_the_underlying_js_function)
 *
 * Because we start with Reason as the source of truth for libraries, we have
 * all the type information for callbacks.
 *
 * Use case:
 * ---------
 *
 * Module.re
 *
 *     [@genFlow]
 *     let consumeCallback = cb => cb(~y=10, ());
 *
 * Consumer.js
 *
 *     const result = Module.consumeCallback(jsObj => jsObj.x + jsObj.y);
 *
 * TODO: - Create Tests Cases With Mismatched Arity
 * TODO:   - Possibly inject runtime .length checks/adapters.
 * TODO: - Map optional types to optional fields.
 * TODO: - Make any final unit args implicit (just as it is right now with BS
 *         FFI for [LOWPRI].
 */
let convertFunToReason = (origFun, groupedArgConverters, retConverter) => {
  let rec createFun = (revAppArgsSoFar, groupedArgConverters) =>
    switch (groupedArgConverters) {
    | [] =>
      let return =
        GenFlowEmitAst.mkExprApplyFunLabels(
          origFun,
          List.rev(revAppArgsSoFar),
        );
      retConverter.toReason(return);
    | [Arg(c), ...tl] =>
      let newIdent = GenIdent.argIdent();
      let nextApp = (
        ReasonAst.Asttypes.Nolabel,
        c.toJS(GenFlowEmitAst.mkExprIdentifier(newIdent)),
      );
      GenFlowEmitAst.mkExprFun(
        newIdent,
        createFun([nextApp, ...revAppArgsSoFar], tl),
      );
    /* We know these several named args will be represented by a single object.
     * The group has a "base" new identifier, and we append the label onto that
     * to determine individual bound pattern identifiers for each argument.
     * This won't work with named arguments that use the same name repeatedly,
     * but that also won't map well to object keys, so that's okay. We need to
     * add a test case for that. */
    | [NamedArgs(nameds), ...tl] =>
      let identBase = GenIdent.argIdent();
      let mapToJSObjRow = ((nextName, optness, c)) => (
        nextName,
        c.toJS(GenFlowEmitAst.mkExprIdentifier(identBase ++ nextName)),
      );
      let jsObj = (
        ReasonAst.Asttypes.Nolabel,
        GenFlowEmitAst.mkJSObj(List.map(mapToJSObjRow, nameds)),
      );
      let revAppArgsWObject = [jsObj, ...revAppArgsSoFar];
      List.fold_right(
        ((nextName, _nextOptness, _), soFar) =>
          GenFlowEmitAst.mkExprFun(
            ~label=ReasonAst.Asttypes.Labelled(nextName),
            identBase ++ nextName,
            soFar,
          ),
        nameds,
        createFun(revAppArgsWObject, tl),
      );
    };
  createFun([], groupedArgConverters);
};

module Convert = {
  let fn = (labeledConverters, retConverter) => {
    let revGroupedArgConverters = groupReversed([], [], labeledConverters);
    let groupedArgConverters = reverse(revGroupedArgConverters);
    {
      toReason: expr =>
        convertFunToReason(expr, groupedArgConverters, retConverter),
      toJS: expr => convertFunToJS(expr, groupedArgConverters, retConverter),
    };
  };

  /**
   * Utility for constructing new optional converters where a specific sentinal
   * value (null/undefined) is used to represent None.
   */
  let optionalConverter = (~jsNoneAs, expressionConverter) => {
    toReason: expr => {
      /* Need to create let binding to avoid double side effects from substitution! */
      let origJSExprIdent = GenIdent.jsMaybeIdent();
      let convertedReasonIdent = GenIdent.optIdent();
      GenFlowEmitAst.(
        mkExprLet(
          origJSExprIdent,
          expr,
          mkExprIf(
            mkTrippleEqualExpr(
              mkExprIdentifier(origJSExprIdent),
              mkJSRawExpr(jsNoneAs),
            ),
            noneExpr,
            mkExprLet(
              convertedReasonIdent,
              expressionConverter.toReason(
                mkExprIdentifier(origJSExprIdent),
              ),
              mkExpr(
                mkExprConstructorDesc(
                  ~payload=mkExprIdentifier(convertedReasonIdent),
                  "Some",
                ),
              ),
            ),
          ),
        )
      );
    },
    toJS: expr =>
      GenFlowEmitAst.(
        mkOptionMatch(expr, mkJSRawExpr(jsNoneAs), ident =>
          expressionConverter.toJS(mkExprIdentifier(ident))
        )
      ),
  };
  let option = optionalConverter(~jsNoneAs="null");
  let optionalArgument = optionalConverter(~jsNoneAs="undefined");
  let unit: expressionConverter = {
    toReason: expr => expr,
    toJS: expr => expr,
  };
  let identity = {toReason: expr => expr, toJS: expr => expr};

  type t =
    | Unit
    | Identity
    | OptionalArgument(t)
    | Option(t)
    | Fn((list((GenFlowCommon.label, t)), t));

  let rec apply = x =>
    switch (x) {
    | Unit => unit
    | Identity => identity
    | OptionalArgument(y) => optionalArgument(y |> apply)
    | Option(y) => option(y |> apply)
    | Fn((y, z)) =>
      fn(y |> List.map(((label, x)) => (label, x |> apply)), z |> apply)
    };
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
type dependency =
  /* Imports a JS Module Value into scope (importAsModuleName, moduleName) */
  | JSModuleImport(string, string)
  /* Import a type that we expect to also be genFlow'd. */
  | TypeAtPath(Path.t)
  /* Imports a JS type (typeName, importAs, moduleName) */
  | JSTypeFromModule(string, string, string)
  /* (type variable name, unique type id) */
  | FreeTypeVariable(string, int);

type convertableFlowType = (Convert.t, Flow.typ);

type conversionPlan = (list(dependency), convertableFlowType);

let hasSomeGADTLeaf = constructorDeclarations =>
  List.exists(
    declaration => declaration.Types.cd_res !== None,
    constructorDeclarations,
  );

let createVariantFunction = (convertableFlowTypes, modPath, leafName) => {
  let rec buildUp =
          (argLen, revConvertedArgs, convertableFlowTypes, modPath, leafName) =>
    switch (revConvertedArgs, convertableFlowTypes) {
    | ([], []) =>
      GenFlowEmitAst.mkExprConstructorDesc(modPath ++ "." ++ leafName)
    | ([hd, ...tl], []) =>
      GenFlowEmitAst.mkExprConstructorDesc(
        ~payload=GenFlowEmitAst.mkTuple(List.rev(revConvertedArgs)),
        modPath ++ "." ++ leafName,
      )
    | (_, [(converter, _), ...tl]) =>
      /* TODO: Apply the converter if available. */
      let maker = GenFlowEmitAst.(tl === [] ? mkExprExplicitArity : mkExpr);
      let name = GenIdent.argIdent();
      let argExpr =
        (converter |> Convert.apply).toReason(
          GenFlowEmitAst.mkExprIdentifier(name),
        );
      GenFlowEmitAst.mkExprFunDesc(
        name,
        maker(
          buildUp(
            argLen + 1,
            [argExpr, ...revConvertedArgs],
            tl,
            modPath,
            leafName,
          ),
        ),
      );
    };
  buildUp(0, [], List.rev(convertableFlowTypes), modPath, leafName);
};

let createFunctionFlowType =
    (generics, argConvertableFlowTypes, resultFlowType) =>
  if (argConvertableFlowTypes === []) {
    resultFlowType;
  } else {
    let args = List.map(((_, flowTyp)) => flowTyp, argConvertableFlowTypes);
    Flow.Arrow(generics, args, resultFlowType);
  };

let rec typePathsEqual = (a, b) =>
  switch (a, b) {
  | (Path.Pident(idA), Path.Pident(idB)) =>
    Ident.name(idA) == Ident.name(idB)
  | (Pdot(pA, sA, _), Pdot(pB, sB, _)) =>
    sA == sB && typePathsEqual(pA, pB)
  | (Path.Papply(_), Path.Papply(_))
  | (_, _) => false
  };

let rec typePathToFlowName = typePath =>
  switch (typePath) {
  | Path.Pident(id) => String.capitalize(Ident.name(id))
  | Pdot(p, s, _pos) => typePathToFlowName(p) ++ String.capitalize(s)
  | Papply(p1, p2) =>
    typePathToFlowName(p1)
    ++ "__unsupported_genFlow__"
    ++ typePathToFlowName(p2)
  };

let importString = (jsTypeName, jsTypeNameAs, jsModuleName) =>
  "import type {"
  ++ jsTypeName
  ++ " as "
  ++ jsTypeNameAs
  ++ "} from '"
  ++ jsModuleName
  ++ "'";

let typePathToFlowImportString = (modulesMap, typePath) =>
  switch (typePath) {
  | Path.Pident(id) when Ident.name(id) == "list" =>
    importString("List", "List", "ReasonPervasives.bs")
  | Path.Pident(id) =>
    "// No need to import locally visible type "
    ++ String.capitalize(Ident.name(id))
    ++ ". Make sure it is also marked with @genFlow"
  | Pdot(p, s, _pos) =>
    importString(
      String.capitalize(s),
      typePathToFlowName(typePath),
      Generator.(
        jsModuleNameForReasonModuleName(
          modulesMap,
          outputReasonModuleName(typePathToFlowName(p)),
        )
      ),
    )
  | Papply(p1, p2) => "// Cannot import type with Papply"
  };

let nullyToOptionalConverter = nullyValue => {};

let needsArgConversion = ((lbl, c)) =>
  lbl !== Nolabel || c !== Convert.Identity;

/**
 * Turns
 *
 *     [(x, (a, b)), (y, (c, d)), ...]
 *
 * Into:
 *
 *     (
 *       [(x, a), (y, c), ...],
 *       [(x, b), (y, d), ...]
 *     )
 */
let rec distributeSplitRev_ = (revLeftSoFar, revRightSoFar, lst) =>
  switch (lst) {
  | [] => (revLeftSoFar, revRightSoFar)
  | [(toDistribute, (ontoA, ontoB)), ...tl] =>
    distributeSplitRev_(
      [(toDistribute, ontoA), ...revLeftSoFar],
      [(toDistribute, ontoB), ...revRightSoFar],
      tl,
    )
  };

let distributeSplitRev = lst => distributeSplitRev_([], [], lst);

let distributeSplit = lst => {
  let (left, right) = distributeSplitRev(lst);
  (List.rev(left), List.rev(right));
};

let variantLeafTypeName = (typeName, leafName) =>
  String.capitalize(typeName) ++ String.capitalize(leafName);

let rec extract_fun = (revArgDeps, revArgs, typ) =>
  Types.(
    switch (typ.desc) {
    | Tlink(t) => extract_fun(revArgDeps, revArgs, t)
    | Tarrow("", t1, t2, _) =>
      let (deps, convertableFlowType) = reasonTypeToConversion(t1);
      let nextRevDeps = List.append(deps, revArgDeps);
      extract_fun(
        nextRevDeps,
        [(Nolabel, convertableFlowType), ...revArgs],
        t2,
      );
    | Tarrow(lbl, t1, t2, _) =>
      switch (removeOption(lbl, t1)) {
      | None =>
        /* TODO: Convert name to object, convert null to optional. */
        let (deps, t1Conversion) = reasonTypeToConversion(t1);
        let nextRevDeps = List.rev_append(deps, revArgDeps);
        extract_fun(
          nextRevDeps,
          [(Label(lbl), t1Conversion), ...revArgs],
          t2,
        );
      | Some((lbl, t1)) =>
        let (deps, (t1Converter, t1FlowType)) = reasonTypeToConversion(t1);
        let t1Conversion = (
          Convert.OptionalArgument(t1Converter),
          t1FlowType,
        );
        let nextRevDeps = List.append(deps, revArgDeps);
        /* TODO: Convert name to object, convert null to optional. */
        extract_fun(
          nextRevDeps,
          [(OptLabel(lbl), t1Conversion), ...revArgs],
          t2,
        );
      }
    | _ =>
      let (retDeps, (retConverter, retType)) = reasonTypeToConversion(typ);
      let (labeledConverters, labeledFlow) = distributeSplitRev(revArgs);
      /* TODO: Ignore all final single unit args at convert/type conversion time. */
      let notJustASingleUnitArg =
        switch (labeledConverters) {
        | [(Nolabel, c)] when c === Convert.Unit => false
        | _ => true
        };
      let needsArgConversion =
        List.exists(needsArgConversion, labeledConverters)
        && notJustASingleUnitArg;
      let allDeps = List.append(List.rev(revArgDeps), retDeps);
      let revGroupedFlow = groupReversed([], [], labeledFlow);
      let groupedFlow = reverse(revGroupedFlow);
      let flowArgs = itm =>
        switch (itm) {
        | NamedArgs(rows) => Flow.ObjectType(rows)
        | Arg(flowType) => flowType
        };
      let flowArrow =
        Flow.Arrow([], List.map(flowArgs, groupedFlow), retType);
      let functionConverter =
        retConverter !== Convert.Identity || needsArgConversion ?
          Convert.Fn((labeledConverters, retConverter)) : Convert.Identity;
      (allDeps, (functionConverter, flowArrow));
    }
  )
/**
 * Convertes Types.type_expr to:
 *
 *   (list(dependency), option(expressionConverter), renderedFlowType)).
 *
 * - `list(dependency)`: The Reason types types used in the type structure.  It
 * must be ensured that these are then imported.
 * - `option(expressionConverter)`: Any converter that is required for this type.
 * - `renderedFlowType` the flow type (just a string prepresentation)
 * TODO: Handle the case where the function in Reason accepts a single unit
 * arg, which should NOT be converted.
 */
and reasonTypeToConversion = (typ: Types.type_expr): conversionPlan =>
  Types.(
    switch (typ.desc) {
    | Tvar(None) =>
      let typeName = Generator.jsTypeNameForAnonymousTypeID(typ.id);
      (
        [FreeTypeVariable(typeName, typ.id)],
        (Convert.Identity, Flow.Ident(typeName, [])),
      );
    | Tvar(Some(s)) =>
      let typeName = s;
      (
        [FreeTypeVariable(typeName, typ.id)],
        (Convert.Identity, Flow.Ident(s, [])),
      );
    | Tconstr(Pdot(Path.Pident({Ident.name: "FB"}), "bool", _), [], _)
    | Tconstr(Path.Pident({name: "bool"}), [], _) => (
        [],
        (Convert.Identity, Flow.Ident("bool", [])),
      )
    | Tconstr(Pdot(Path.Pident({Ident.name: "FB"}), "int", _), [], _)
    | Tconstr(Path.Pident({name: "int"}), [], _) => (
        [],
        (Convert.Identity, Flow.Ident("number", [])),
      )
    | Tconstr(Pdot(Path.Pident({Ident.name: "FB"}), "string", _), [], _)
    | Tconstr(Path.Pident({name: "string"}), [], _) => (
        [],
        (Convert.Identity, Flow.Ident("string", [])),
      )
    | Tconstr(Pdot(Path.Pident({Ident.name: "FB"}), "unit", _), [], _)
    | Tconstr(Path.Pident({name: "unit"}), [], _) => (
        [],
        (Convert.Unit, Flow.Ident("(typeof undefined)", [])),
      )
    /*
     * Arrays do not experience any conversion, in order to retain referencial
     * equality. This poses a problem for Arrays that contain option types
     * which require conversion. The solution here could be to use the Reason
     * representation of option for everything except named arguments.
     */
    | Tconstr(Pdot(Path.Pident({Ident.name: "FB"}), "array", _), [p], _)
    | Tconstr(Path.Pident({name: "array"}), [p], _) =>
      let (paramDeps, (itemConverter, itemFlow)) =
        reasonTypeToConversion(p);
      if (itemConverter === Convert.Identity) {
        (
          paramDeps,
          (Convert.Identity, Flow.Ident("$ReadOnlyArray", [itemFlow])),
        );
      } else {
        raise(
          Invalid_argument(
            "Converting Arrays with elements that require conversion "
            ++ "is not yet supported. Saw an array containing type:"
            ++ Flow.render(itemFlow),
          ),
        );
      };
    | Tconstr(Pdot(Path.Pident({Ident.name: "FB"}), "option", _), [p], _)
    | Tconstr(Path.Pident({name: "option"}), [p], _) =>
      /* TODO: Handle / verify the case of nested optionals. */
      let (paramDeps, (paramConverter, paramConverted)) =
        reasonTypeToConversion(p);
      let composedConverter = Convert.Option(paramConverter);
      (paramDeps, (composedConverter, Flow.Optional(paramConverted)));
    | Tarrow(_) => extract_fun([], [], typ)
    | Tlink(t) => reasonTypeToConversion(t)
    | Tconstr(path, [], _) => (
        [TypeAtPath(path)],
        (Convert.Identity, Flow.Ident(typePathToFlowName(path), [])),
      )
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
      let (typeParamDeps, convertableFlowType) =
        reasonTypeToConversionMany(typeParams);
      /* How is this exprConv completely ignored? */
      let typeArgs =
        List.map(
          ((exprConv, flowTyp: Flow.typ)) => flowTyp,
          convertableFlowType,
        );
      (
        [TypeAtPath(path), ...typeParamDeps],
        (Convert.Identity, Flow.Ident(typePathToFlowName(path), typeArgs)),
      );
    | _ => ([], (Convert.Identity, Flow.anyAlias))
    }
  )
and reasonTypeToConversionMany = args => {
  let (deps, convertableFlowTypes) =
    List.split(List.map(reasonTypeToConversion, args));
  (List.concat(deps), convertableFlowTypes);
};

let pp = Printf.fprintf;

module Dependencies = {
  /**
   * Allows checking if there exists a polymorphic dep before creating several
   * list coppies.
   */
  let rec hasTypeVar = deps =>
    switch (deps) {
    | [] => false
    | [FreeTypeVariable(_s, _), ...tl] => true
    | [_, ...tl] => hasTypeVar(tl)
    };
  /*
   * A little bit of n squared never hurt anyone for n < 5.
   */
  let extractFreeTypeVars = deps =>
    if (hasTypeVar(deps)) {
      List.fold_left(
        ((curFreeTypeVars, curNonFreeTypeVars) as soFar, next) =>
          switch (next) {
          | FreeTypeVariable(s, id) =>
            List.exists(((s2, id2)) => s2 == s, curFreeTypeVars) ?
              soFar : ([(s, id), ...curFreeTypeVars], curNonFreeTypeVars)
          | _ => (curFreeTypeVars, [next, ...curNonFreeTypeVars])
          },
        ([], []),
        deps,
      );
    } else {
      ([], deps);
    };
  let filterFreeTypeVars = (freeTypeVars, deps) =>
    List.filter(
      fun
      | FreeTypeVariable(s, id) =>
        !List.exists(((s2, id2)) => id == id2, freeTypeVars)
      | _ => true,
      deps,
    );
};

module TypeVars = {
  /**
   * Extracts type variables from dependencies.
   */
  let extractOne = (soFar, typ) =>
    switch (typ) {
    | {Types.id, desc: Tvar(None)} =>
      let typeName = Generator.jsTypeNameForAnonymousTypeID(id);
      [(typeName, id), ...soFar];
    | {id, desc: Tvar(Some(s))} =>
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
    let typeVarnamesAndIDs = List.fold_left(extractOne, [], typeParams);
    List.rev(typeVarnamesAndIDs);
  };
  /*
   * A little bit of n squared never hurt anyone for n < 5.
   */
  let names = freeTypeVars => List.map(((name, id)) => name, freeTypeVars);
  let toFlow = freeTypeVars =>
    List.map(((name, id)) => Flow.Ident(name, []), freeTypeVars);
};

type codeItem =
  | RawJS(string)
  | FlowTypeBinding(string, GenFlowCommon.Flow.typ)
  | FlowAnnotation(string, GenFlowCommon.Flow.typ)
  | ValueBinding(string, Ident.t, Convert.t)
  | ConstructorBinding(string, list(convertableFlowType), string, string)
  | ComponentBinding(
      string,
      option(GenFlowCommon.Flow.typ),
      Ident.t,
      Convert.t,
    );

let codeItemForType = (~opaque=false, typeParams, name, underlying) => {
  let opaqueTypeString =
    "export"
    ++ (opaque ? " opaque " : " ")
    ++ "type "
    ++ String.capitalize(name)
    ++ Flow.genericsString(List.map(Flow.render, typeParams))
    ++ " = "
    ++ Flow.render(underlying)
    ++ (opaque ? " // Reason type already checked. Making it opaque" : "");
  RawJS(opaqueTypeString);
};

let codeItemForOpaqueType = (typeParams, name, underlying) =>
  codeItemForType(~opaque=true, typeParams, name, underlying);

let codeItemForUnionType = (typeParams, leafTypes, name) => {
  let opaqueTypeString =
    "export type "
    ++ String.capitalize(name)
    ++ Flow.genericsString(List.map(Flow.render, typeParams))
    ++ " =\n  | "
    ++ String.concat("\n  | ", List.map(Flow.render, leafTypes));
  RawJS(opaqueTypeString);
};

/*
 * TODO: Make the types namespaced by nested Flow module.
 */
let codeItemsFromConstructorDeclaration =
    (modulePath, variantTypeName, constructorDeclaration) => {
  GenIdent.resetPerStructure();
  let constructorArgs = constructorDeclaration.Types.cd_args;
  let leafName = Ident.name(constructorDeclaration.Types.cd_id);
  let lowercaseLeaf = String.uncapitalize(leafName);
  let (deps, convertableFlowTypes) =
    reasonTypeToConversionMany(constructorArgs);
  /* A valid Reason identifier that we can point UpperCase JS exports to. */
  let constructorAlias =
    BuckleScriptPostProcessLib.Patterns.capitalizeExportedNamePrefix
    ++ lowercaseLeaf;
  let annotationBindingName =
    BuckleScriptPostProcessLib.Patterns.flowTypeAnnotationPrefix
    ++ constructorAlias;
  let leafTypeName = variantLeafTypeName(variantTypeName, leafName);
  let (freeTypeVars, remainingDeps) = Dependencies.extractFreeTypeVars(deps);
  let flowTypeVars = TypeVars.toFlow(freeTypeVars);
  let retType = Flow.Ident(leafTypeName, flowTypeVars);
  let constructorFlowType =
    createFunctionFlowType(flowTypeVars, convertableFlowTypes, retType);
  let codeItems = [
    codeItemForOpaqueType(flowTypeVars, leafTypeName, Flow.anyAlias),
    FlowAnnotation(annotationBindingName, constructorFlowType),
    ConstructorBinding(
      constructorAlias,
      convertableFlowTypes,
      modulePath,
      leafName,
    ),
  ];
  (retType, (remainingDeps, codeItems));
};

let mkFlowTypeBinding = (name, flowType) =>
  GenFlowEmitAst.(
    mkBinding(
      mkPatternIdent(
        BuckleScriptPostProcessLib.Patterns.flowTypeAnnotationPrefix ++ name,
      ),
      mkExpr(
        ReasonAst.Parsetree.Pexp_constant(
          Pconst_string(Flow.render(flowType), None),
        ),
      ),
    )
  );

let codeItemsForId = (~inputModuleName, ~valueBinding, id) => {
  let {Typedtree.vb_expr} = valueBinding;
  let expressionType = vb_expr.exp_type;
  let conversion = reasonTypeToConversion(expressionType);
  let (valueDeps, (converter, flowType)) = conversion;
  /*
   * We pull apart the polymorphic type variables at the binding level, but
   * not at deeper function types because we know that the Reason/OCaml type
   * system doesn't support higher ranked polymorphism, and so all type
   * variables most likely belong at the binding level.
   */
  let (freeTypeVars, remainingDeps) =
    Dependencies.extractFreeTypeVars(valueDeps);
  let flowTypeVars = TypeVars.toFlow(freeTypeVars);
  let flowType = Flow.abstractTheTypeParameters(flowType, flowTypeVars);
  let codeItems = [
    FlowTypeBinding(Ident.name(id), flowType),
    ValueBinding(inputModuleName, id, converter),
  ];
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

let codeItemsForMake = (~inputModuleName, ~valueBinding, id) => {
  let {Typedtree.vb_expr} = valueBinding;
  let expressionType = vb_expr.exp_type;
  let conversion = reasonTypeToConversion(expressionType);
  let (valueDeps, (converter, flowType)) = conversion;
  let (freeTypeVars, remainingDeps) =
    Dependencies.extractFreeTypeVars(valueDeps);
  let flowTypeVars = TypeVars.toFlow(freeTypeVars);
  let flowType = Flow.abstractTheTypeParameters(flowType, flowTypeVars);
  switch (flowType) {
  | Flow.Arrow(
      _,
      [propOrChildren, ...childrenOrNil],
      Flow.Ident(
        "ReasonReactComponentSpec" | "ReactComponentSpec",
        [state, ..._],
      ),
    ) =>
    let flowPropGenerics =
      switch (childrenOrNil) {
      /* Then we only extracted a function that accepts children, no props */
      | [] => None
      /* Then we had both props and children. */
      | [children, ..._] => Some(propOrChildren)
      };
    let propsTypeName = GenIdent.propsTypeName();
    let propsTypeNameFlow = Flow.Ident(propsTypeName, []);
    /* TODO: Polymorphic props */
    let componentFlowType =
      Flow.Ident(
        "React$ComponentType",
        switch (flowPropGenerics) {
        | None => []
        | Some(propsType) => [propsTypeNameFlow]
        },
      );
    let propsTypeDeclaration =
      switch (flowPropGenerics) {
      | None => []
      | Some(propsType) => [codeItemForType([], propsTypeName, propsType)]
      };

    let items =
      propsTypeDeclaration
      @ [
        FlowTypeBinding("component", componentFlowType),
        ComponentBinding(inputModuleName, flowPropGenerics, id, converter),
      ];
    let deps = [
      JSTypeFromModule("Component", "ReactComponent", "React"),
      ...remainingDeps,
    ];
    (deps, items);
  | _ =>
    /* not a component: treat make as a normal function */
    id |> codeItemsForId(~inputModuleName, ~valueBinding)
  };
};

/**
 * See how this binding is accessed/generated.
 * [@bs.module] external workModeConfig : Js.t('untypedThing) = "WorkModeConfig";
 * Js_unsafe.raw_expr("require('MyModule')");
 */
/**
 * This is where all the logic for mapping from a CMT file, to a parsetree Ast
 * which has injected Flow types and generated interop code.
 * Each @genFlow binding will have the following two:
 *
 *     let __flowTypeValueAnnotation__bindingName = "someFlowType";
 *     let bindingName =
 *       require('ModuleWhereOrigBindingLives.bs').bindingName;
 *
 * Where the "someFlowType" is a flow converted type from Reason type, and
 * where the require() redirection may perform some safe conversions.
 */
let codeItemsForValueBinding = (~inputModuleName, valueBinding) => {
  let {Typedtree.vb_pat, vb_expr, vb_attributes} = valueBinding;
  GenIdent.resetPerStructure();
  switch (vb_pat.pat_desc, getGenFlowKind(vb_attributes)) {
  | (Tpat_var(id, _), GenFlow) when Ident.name(id) == "make" =>
    id |> codeItemsForMake(~inputModuleName, ~valueBinding)
  | (Tpat_var(id, _), GenFlow) =>
    id |> codeItemsForId(~inputModuleName, ~valueBinding)
  | _ => ([], [])
  };
};

let codeItemsForTypeDecl = (~inputModuleName, dec: Typedtree.type_declaration) => {
  GenIdent.resetPerStructure();
  switch (
    dec.typ_type.type_params,
    dec.typ_type.type_kind,
    getGenFlowKind(dec.typ_attributes),
  ) {
  | (typeParams, Type_record(_, _), GenFlow | GenFlowOpaque) =>
    let freeTypeVars = TypeVars.extract(typeParams);
    let flowTypeVars = TypeVars.toFlow(freeTypeVars);
    let typeName = Ident.name(dec.typ_id);
    ([], [codeItemForOpaqueType(flowTypeVars, typeName, Flow.anyAlias)]);
  /*
   * This case includes aliasings such as:
   *
   *     type list('t) = List.t('t');
   */
  | (typeParams, Type_abstract, GenFlow | GenFlowOpaque)
  | (typeParams, Type_variant(_), GenFlowOpaque) =>
    let freeTypeVars = TypeVars.extract(typeParams);
    let flowTypeVars = TypeVars.toFlow(freeTypeVars);
    let typeName = Ident.name(dec.typ_id);
    switch (dec.typ_manifest) {
    | None => (
        [],
        [codeItemForOpaqueType(flowTypeVars, typeName, Flow.anyAlias)],
      )
    | Some(coreType) =>
      let (deps, (_converter, flowType)) =
        reasonTypeToConversion(coreType.Typedtree.ctyp_type);
      let structureItems = [
        codeItemForOpaqueType(flowTypeVars, typeName, flowType),
      ];
      let deps = Dependencies.filterFreeTypeVars(freeTypeVars, deps);
      (deps, structureItems);
    };
  | (typeParams, Type_variant(constructorDeclarations), GenFlow)
      when !hasSomeGADTLeaf(constructorDeclarations) =>
    let variantTypeName = Ident.name(dec.typ_id);
    let resultTypesDepsAndVariantLeafBindings =
      List.map(
        codeItemsFromConstructorDeclaration(inputModuleName, variantTypeName),
        constructorDeclarations,
      );
    let (resultTypes, depsAndVariantLeafBindings) =
      List.split(resultTypesDepsAndVariantLeafBindings);
    let (listListDeps, listListItems) =
      List.split(depsAndVariantLeafBindings);
    let deps = List.concat(listListDeps);
    let items = List.concat(listListItems);
    let flowTypeVars = TypeVars.toFlow(TypeVars.extract(typeParams));
    let unionType =
      codeItemForUnionType(flowTypeVars, resultTypes, variantTypeName);
    (deps, List.append(items, [unionType]));
  | _ => ([], [])
  };
};

let typedItemToCodeItems = (~inputModuleName, typedItem) => {
  let (listListDeps, listListItems) =
    switch (typedItem) {
    | {Typedtree.str_desc: Typedtree.Tstr_type(typeDeclarations)} =>
      typeDeclarations
      |> List.map(codeItemsForTypeDecl(~inputModuleName))
      |> List.split
    | {Typedtree.str_desc: Tstr_value(loc, valueBindings)} =>
      valueBindings
      |> List.map(codeItemsForValueBinding(~inputModuleName))
      |> List.split
    | _ => ([], [])
    /* TODO: Support mapping of variant type definitions. */
    };
  (List.concat(listListDeps), List.concat(listListItems));
};

let dependencyEqual = (a, b) =>
  switch (a, b) {
  | (TypeAtPath(pA), TypeAtPath(pB)) => typePathsEqual(pA, pB)
  | (JSTypeFromModule(sA, sB, sC), JSTypeFromModule(sD, sE, sF)) =>
    sA == sD && sB == sE && sC == sF
  | (FreeTypeVariable(sA, idA), FreeTypeVariable(sB, idB)) =>
    sA == sB && idA === idB
  | (
      JSModuleImport(importAsA, jsModuleNameA),
      JSModuleImport(importAsB, jsModuleNameB),
    ) =>
    importAsA == importAsB && jsModuleNameA == jsModuleNameB
  | _ => false
  };

let codeItemsForDependencies = (modulesMap, dependencies): list(codeItem) => {
  /*
   * Makes sure we only add a dependency/import at the top of the file once per
   * dependency. How about a little n square action! (These lists should be
   * about length 3-5).
   */
  let folder = ((handledDeps, revCodeItems) as soFar, next) =>
    if (List.exists(dependencyEqual(next), handledDeps)) {
      soFar;
    } else {
      let codeItem =
        switch (next) {
        | TypeAtPath(p) =>
          let importTypeString = typePathToFlowImportString(modulesMap, p);
          RawJS(importTypeString);
        | JSTypeFromModule(typeName, asName, moduleName) =>
          let importTypeString = importString(typeName, asName, moduleName);
          RawJS(importTypeString);
        | FreeTypeVariable(s, id) =>
          RawJS("// Warning polymorphic type unhandled:" ++ s)
        /* TODO: Currently unused. Would be useful for injecting dependencies
         * on runtime converters that end up being used. */
        | JSModuleImport(importAs, jsModuleName) =>
          RawJS(
            "const " ++ importAs ++ " = require('" ++ jsModuleName ++ "')",
          )
        };
      ([next, ...handledDeps], [codeItem, ...revCodeItems]);
    };
  let (handledDeps, revCodeItems) =
    List.fold_left(folder, ([], []), dependencies);
  revCodeItems;
};

let cmtToCodeItems =
    (~modulesMap, ~globalModuleName, inputCMT): list(codeItem) => {
  let {Cmt_format.cmt_annots} = inputCMT;
  switch (cmt_annots) {
  | Implementation(structure) =>
    let typedItems = structure.Typedtree.str_items;
    let (deps, codeItems) =
      List.fold_left(
        ((curDeps, curParseItems), nextTypedItem) => {
          let (nextDeps, nextCodeItems) =
            nextTypedItem
            |> typedItemToCodeItems(~inputModuleName=globalModuleName);
          (nextDeps @ curDeps, nextCodeItems @ curParseItems);
        },
        ([], []),
        typedItems,
      );
    let imports = codeItemsForDependencies(modulesMap, deps);
    List.append(imports, codeItems);
  | _ => []
  };
};

let log = Printf.printf;
let logItem = x => {
  log("  > ");
  log(x);
};

module GeneratedReFiles = {
  type t = {
    filesOnDisk: StringSet.t,
    mutable filesToWrite: StringSet.t,
  };

  type fileAction =
    | NoMatch /* No @genFlow annotation found. */
    | Replace /* Replace existing file on disk with new contents. */
    | Skip /* File already on disk with identical contents. */
    | Write; /* File not present on disk. */

  let logFileAction = (fileAction, fileName) =>
    logItem(
      "%s  %s\n",
      switch (fileAction) {
      | NoMatch => "NoMatch"
      | Replace => "Replace"
      | Skip => "Skip   "
      | Write => "Write  "
      },
      fileName,
    );

  let readFromDisk = (~outputDir) => {
    let filesOnDisk =
      outputDir
      |> Sys.readdir
      |> Array.fold_left(
           (set, file) =>
             Filename.check_suffix(file, ".re") ?
               StringSet.add(Filename.concat(outputDir, file), set) : set,
           StringSet.empty,
         );
    logItem(
      "Found %d generated .re files in %s\n",
      filesOnDisk |> StringSet.cardinal,
      outputDir,
    );
    {filesOnDisk, filesToWrite: StringSet.empty};
  };

  let readLines = (file: string): list(string) => {
    let lines = ref([]);
    let chan = open_in(file);
    let finished_lines =
      try (
        {
          while (true) {
            lines := [input_line(chan), ...lines^];
          };
          [];
        }
      ) {
      | End_of_file =>
        close_in(chan);
        lines^ |> List.rev;
      };
    finished_lines;
  };

  let readFile = (file: string): string =>
    String.concat("\n", readLines(file));

  let writeFileIfRequired = (~fileName, ~fileContents, ~writeFile, x) => {
    x.filesToWrite = StringSet.add(fileName, x.filesToWrite);
    if (StringSet.mem(fileName, x.filesOnDisk)) {
      let oldContents = readFile(fileName);
      let identical = oldContents == fileContents;
      if (identical) {
        fileName |> logFileAction(Skip);
      };
      if (!identical) {
        fileName |> logFileAction(Replace);
        writeFile(fileName, fileContents);
      };
    } else {
      fileName |> logFileAction(Write);
      writeFile(fileName, fileContents);
    };
  };

  let cleanup = ({filesOnDisk, filesToWrite}) => {
    let filesToRemove = StringSet.diff(filesOnDisk, filesToWrite);
    if (!StringSet.is_empty(filesToRemove)) {
      logItem("Clean up %d .re files\n", filesToRemove |> StringSet.cardinal);
      StringSet.iter(
        file => {
          log("Delete %s\n", file);
          Unix.unlink(file);
        },
        filesToRemove,
      );
    } else {
      logItem("No .re files to clean up.\n");
    };
  };
};

let writeFile = (filePath: string, contents: string) => {
  let outFile = open_out(filePath);
  output_string(outFile, contents);
  close_out(outFile);
};

let emitCodeItems =
    (
      ~generatedFiles,
      ~inputPath,
      ~outputPath,
      ~fileHeader,
      ~signFile,
      structureItems,
    ) =>
  switch (structureItems) {
  | [_, ..._] =>
    let emitStructureItem = structureItem => {
      let outputFormatter = Format.str_formatter;
      Reason_toolchain.RE.print_implementation_with_comments(
        outputFormatter,
        ([structureItem], []),
      );
      Format.pp_print_flush(outputFormatter, ());
      Format.flush_str_formatter();
    };
    let emitCodeItem = codeItem =>
      switch (codeItem) {
      | RawJS(s) => "Js_unsafe.raw_stmt(\n  \"" ++ s ++ "\",\n);\n"
      | FlowTypeBinding(id, flowType) =>
        [mkFlowTypeBinding(id, flowType)]
        |> GenFlowEmitAst.mkStructItemValBindings
        |> emitStructureItem
      | FlowAnnotation(annotationBindingName, constructorFlowType) =>
        GenFlowEmitAst.mkFlowAnnotationStructItem(
          annotationBindingName,
          constructorFlowType,
        )
        |> emitStructureItem
      | ValueBinding(inputModuleName, id, converter) =>
        let consumeProp =
          GenFlowEmitAst.mkExprIdentifier(
            inputModuleName ++ "." ++ Ident.name(id),
          );
        GenFlowEmitAst.mkStructItemValBindings([
          GenFlowEmitAst.mkBinding(
            GenFlowEmitAst.mkPatternIdent(Ident.name(id)),
            (converter |> Convert.apply).toJS(consumeProp),
          ),
        ])
        |> emitStructureItem;
      | ConstructorBinding(
          constructorAlias,
          convertableFlowTypes,
          modulePath,
          leafName,
        ) =>
        GenFlowEmitAst.mkStructItemValBindings([
          GenFlowEmitAst.mkBinding(
            GenFlowEmitAst.mkPattern(
              Ppat_var(GenFlowEmitAst.located(constructorAlias)),
            ),
            GenFlowEmitAst.mkExpr(
              createVariantFunction(
                convertableFlowTypes,
                modulePath,
                leafName,
              ),
            ),
          ),
        ])
        |> emitStructureItem
      | ComponentBinding(inputModuleName, flowPropGenerics, id, converter) =>
        let makeIdentifier =
          GenFlowEmitAst.mkExprIdentifier(
            inputModuleName ++ "." ++ Ident.name(id),
          );
        let jsPropsIdent = GenFlowEmitAst.mkExprIdentifier("jsProps");
        let getChildrenFromJSProps =
          GenFlowEmitAst.mkJSGet(jsPropsIdent, "children");
        let jsPropsToReason =
          GenFlowEmitAst.mkExprFun(
            "jsProps",
            GenFlowEmitAst.mkExprApplyFun(
              (converter |> Convert.apply).toJS(makeIdentifier),
              flowPropGenerics == None ?
                [jsPropsIdent] : [jsPropsIdent, getChildrenFromJSProps],
            ),
          );
        GenFlowEmitAst.mkStructItemValBindings([
          GenFlowEmitAst.mkBinding(
            GenFlowEmitAst.mkPatternIdent("component"),
            GenFlowEmitAst.mkExprApplyFunLabels(
              GenFlowEmitAst.mkExprIdentifier("ReasonReact.wrapReasonForJs"),
              [
                (
                  ReasonAst.Asttypes.Labelled("component"),
                  GenFlowEmitAst.mkExprIdentifier(
                    inputModuleName ++ "." ++ "component",
                  ),
                ),
                (ReasonAst.Asttypes.Nolabel, jsPropsToReason),
              ],
            ),
          ),
        ])
        |> emitStructureItem;
      };
    let astText =
      structureItems |> List.map(emitCodeItem) |> String.concat("");
    let astTextNoNewline = {
      /* refmt would also remove the newline */
      let n = String.length(astText);
      n > 0 && astText.[(n - 1)] == '\n' ?
        String.sub(astText, 0, n - 1) : astText;
    };
    let fileContents = signFile(fileHeader ++ astTextNoNewline);

    generatedFiles
    |> GeneratedReFiles.writeFileIfRequired(
         ~fileName=outputPath,
         ~fileContents,
         ~writeFile,
       );

  | [] => outputPath |> GeneratedReFiles.logFileAction(NoMatch)
  };

let processCMTFile =
    (
      ~generatedFiles,
      ~modulesMap,
      ~outputDir,
      ~fileHeader,
      ~signFile,
      inputPath,
    ) => {
  GenIdent.resetPerFile();
  let inputCMT = Cmt_format.read_cmt(inputPath);
  let globalModuleName =
    Filename.chop_extension(Filename.basename(inputPath));
  let outputPath =
    Filename.concat(
      outputDir,
      Generator.outputReasonModuleName(globalModuleName) ++ ".re",
    );
  inputCMT
  |> cmtToCodeItems(~modulesMap, ~globalModuleName)
  |> emitCodeItems(
       ~generatedFiles,
       ~inputPath,
       ~outputPath,
       ~fileHeader,
       ~signFile,
     );
};

let run =
    (
      ~outputDir,
      ~fileHeader,
      ~signFile,
      ~modulesMap,
      ~findCmtFiles,
      ~buildSourceFiles,
      ~buildGeneratedFiles,
      ~doCleanup,
    ) => {
  buildSourceFiles();

  log("Looking for files with %s\n", Generator.tagSearch);
  let cmtFiles = findCmtFiles();
  cmtFiles |> List.iter(fileName => logItem("Found  %s\n", fileName));

  log("Searching for existing files on disk\n");
  let generatedFiles = GeneratedReFiles.readFromDisk(~outputDir);

  log("Generating .re files\n");
  cmtFiles
  |> List.iter(
       processCMTFile(
         ~generatedFiles,
         ~modulesMap,
         ~fileHeader,
         ~signFile,
         ~outputDir,
       ),
     );

  if (doCleanup) {
    log("Cleaning up\n");
    GeneratedReFiles.cleanup(generatedFiles);
  };

  buildGeneratedFiles();
  log("Done\n");
};