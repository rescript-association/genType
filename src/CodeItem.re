open GenFlowCommon;

type dependency =
  /* Import a type that we expect to also be genFlow'd. */
  | TypeAtPath(Path.t)
  /* Imports a JS type (typeName, importAs, jsModuleName) */
  | JSTypeFromModule(string, option(string), ImportPath.t)
  /* (type variable name, unique type id) */
  | FreeTypeVariable(string, int);

type conversionPlan = {
  dependencies: list(dependency),
  convertableType,
};

type import =
  | ImportComment(string)
  | ImportAsFrom(string, option(string), ImportPath.t);

type exportType = {
  opaque: bool,
  typeParams: list(typ),
  typeName: string,
  typ,
};

type exportUnionType = {
  typeParams: list(typ),
  leafTypes: list(typ),
  name: string,
};

type componentBinding = {
  moduleName: ModuleName.t,
  componentType: typ,
  propsTypeName: string,
  converter,
};

type externalReactClass = {
  componentName: string,
  importPath: ImportPath.t,
};

type t =
  | ImportType(import)
  | ExternalReactClass(externalReactClass)
  | ValueBinding(ModuleName.t, string, typ, converter)
  | ConstructorBinding(
      typ,
      list(convertableType),
      string,
      Runtime.recordValue,
    )
  | ComponentBinding(componentBinding)
  | ExportType(exportType)
  | ExportUnionType(exportUnionType);

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
      | Label(l) => "~l"
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
               |> List.map(((s, optionalness, argConverter)) =>
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

let toString = codeItem =>
  switch (codeItem) {
  | ImportType(_) => "ImportType"
  | ExternalReactClass(_) => "ExternalReactClass"
  | ValueBinding(moduleName, id, flowType, converter) =>
    "ValueBinding"
    ++ " id:"
    ++ id
    ++ " flowType:"
    ++ EmitFlow.toString(flowType)
    ++ " converter:"
    ++ converterToString(converter)
  | ConstructorBinding(_) => "ConstructorBinding"
  | ComponentBinding(_) => "ComponentBinding"
  | ExportType(_) => "ExportType"
  | ExportUnionType(_) => "ExportUnionType"
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
let rec distributeSplitRev_ = (revSoFar, lst) =>
  switch (lst) {
  | [] => revSoFar
  | [(toDistribute, (ontoA, ontoB)), ...tl] =>
    distributeSplitRev_([(toDistribute, (ontoA, ontoB)), ...revSoFar], tl)
  };

let distributeSplitRev = lst => distributeSplitRev_([], lst);

let rec typePathToFlowName = typePath =>
  switch (typePath) {
  | Path.Pident(id) => Ident.name(id)
  | Pdot(p, s, _pos) => typePathToFlowName(p) ++ s
  | Papply(p1, p2) =>
    typePathToFlowName(p1)
    ++ "__unsupported_genFlow__"
    ++ typePathToFlowName(p2)
  };

let rec extract_fun = (revArgDeps, revArgs, typ) =>
  Types.(
    switch (typ.desc) {
    | Tlink(t) => extract_fun(revArgDeps, revArgs, t)
    | Tarrow("", t1, t2, _) =>
      let {dependencies, convertableType} = reasonTypeToConversion(t1);
      let nextRevDeps = List.append(dependencies, revArgDeps);
      extract_fun(
        nextRevDeps,
        [(Nolabel, convertableType), ...revArgs],
        t2,
      );
    | Tarrow(lbl, t1, t2, _) =>
      switch (removeOption(lbl, t1)) {
      | None =>
        /* TODO: Convert name to object, convert null to optional. */
        let {dependencies, convertableType: t1Conversion} =
          reasonTypeToConversion(t1);
        let nextRevDeps = List.rev_append(dependencies, revArgDeps);
        extract_fun(
          nextRevDeps,
          [(Label(lbl), t1Conversion), ...revArgs],
          t2,
        );
      | Some((lbl, t1)) =>
        let {dependencies, convertableType: (t1Converter, t1FlowType)} =
          reasonTypeToConversion(t1);
        let t1Conversion = (OptionalArgument(t1Converter), t1FlowType);
        let nextRevDeps = List.append(dependencies, revArgDeps);
        /* TODO: Convert name to object, convert null to optional. */
        extract_fun(
          nextRevDeps,
          [(OptLabel(lbl), t1Conversion), ...revArgs],
          t2,
        );
      }
    | _ =>
      let {dependencies, convertableType: (retConverter, retType)} =
        reasonTypeToConversion(typ);
      let allDeps = List.append(List.rev(revArgDeps), dependencies);

      let labeledConvertableTypes = distributeSplitRev(revArgs);
      let groupedArgs = labeledConvertableTypes |> NamedArgs.group;

      let groupedArgToConverter = groupedArg =>
        switch (groupedArg) {
        | Group(group) =>
          GroupConverter(
            group
            |> List.map(((s, optionalness, (c, _t))) =>
                 (s, optionalness, c)
               ),
          )
        | Arg((c, _t)) => ArgConverter(Nolabel, c)
        };
      let groupedArgConverters =
        groupedArgs |> List.map(groupedArgToConverter);
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
        | Arg((_c, flowType)) => flowType
        };
      let functionType =
        Arrow([], groupedArgs |> List.map(groupedArgToTyp), retType);

      {
        dependencies: allDeps,
        convertableType: (functionConverter, functionType),
      };
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
      let typeName = jsTypeNameForAnonymousTypeID(typ.id);
      {
        dependencies: [FreeTypeVariable(typeName, typ.id)],
        convertableType: (Identity, Ident(typeName, [])),
      };
    | Tvar(Some(s)) =>
      let typeName = s;
      {
        dependencies: [FreeTypeVariable(typeName, typ.id)],
        convertableType: (Identity, Ident(s, [])),
      };
    | Tconstr(Pdot(Path.Pident({Ident.name: "FB", _}), "bool", _), [], _)
    | Tconstr(Path.Pident({name: "bool", _}), [], _) => {
        dependencies: [],
        convertableType: (Identity, Ident("bool", [])),
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
      let {
        dependencies: paramDeps,
        convertableType: (itemConverter, itemFlow),
      } =
        reasonTypeToConversion(p);
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
            ++ EmitFlow.toString(itemFlow),
          ),
        );
      };
    | Tconstr(
        Pdot(Path.Pident({Ident.name: "FB", _}), "option", _),
        [p],
        _,
      )
    | Tconstr(Path.Pident({name: "option", _}), [p], _) =>
      /* TODO: Handle / verify the case of nested optionals. */
      let {
        dependencies: paramDeps,
        convertableType: (paramConverter, paramConverted),
      } =
        reasonTypeToConversion(p);
      let composedConverter = Option(paramConverter);
      {
        dependencies: paramDeps,
        convertableType: (composedConverter, Optional(paramConverted)),
      };
    | Tarrow(_) => extract_fun([], [], typ)
    | Tlink(t) => reasonTypeToConversion(t)
    | Tconstr(path, [], _) => {
        dependencies: [TypeAtPath(path)],
        convertableType: (Identity, Ident(typePathToFlowName(path), [])),
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
      let conversionPlans = reasonTypesToConversion(typeParams);
      let convertableFlowTypes =
        conversionPlans
        |> List.map(({convertableType, _}) => convertableType);
      let typeParamDeps =
        conversionPlans
        |> List.map(({dependencies, _}) => dependencies)
        |> List.concat;
      /* How is this exprConv completely ignored? */
      let typeArgs =
        List.map(
          ((_exprConv, flowTyp: typ)) => flowTyp,
          convertableFlowTypes,
        );
      {
        dependencies: [TypeAtPath(path), ...typeParamDeps],
        convertableType: (
          Identity,
          Ident(typePathToFlowName(path), typeArgs),
        ),
      };
    | _ => {dependencies: [], convertableType: (Identity, EmitFlow.any)}
    }
  )
and reasonTypesToConversion = args: list(conversionPlan) =>
  args |> List.map(reasonTypeToConversion);

module Dependencies = {
  /**
     * Allows checking if there exists a polymorphic dep before creating several
     * list coppies.
     */
  let rec hasTypeVar = deps =>
    switch (deps) {
    | [] => false
    | [FreeTypeVariable(_s, _), ..._tl] => true
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
            List.exists(((s2, _id2)) => s2 == s, curFreeTypeVars) ?
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
      | FreeTypeVariable(_s, id) =>
        !List.exists(((_s2, id2)) => id == id2, freeTypeVars)
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
    | {Types.id, desc: Tvar(None), _} =>
      let typeName = jsTypeNameForAnonymousTypeID(id);
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
    let typeVarnamesAndIDs = List.fold_left(extractOne, [], typeParams);
    List.rev(typeVarnamesAndIDs);
  };
  /*
   * A little bit of n squared never hurt anyone for n < 5.
   */
  let names = freeTypeVars => List.map(((name, _id)) => name, freeTypeVars);
  let toFlow = freeTypeVars =>
    List.map(((name, _id)) => Ident(name, []), freeTypeVars);
};

let createFunctionFlowType =
    (generics, argConvertableFlowTypes, resultFlowType) =>
  if (argConvertableFlowTypes === []) {
    resultFlowType;
  } else {
    let args = List.map(((_, flowTyp)) => flowTyp, argConvertableFlowTypes);
    Arrow(generics, args, resultFlowType);
  };

let codeItemForType = (~opaque, typeParams, ~typeName, typ) =>
  ExportType({opaque, typeParams, typeName, typ});

let variantLeafTypeName = (typeName, leafName) =>
  String.capitalize(typeName) ++ String.capitalize(leafName);

/*
 * TODO: Make the types namespaced by nested Flow module.
 */
let codeItemsFromConstructorDeclaration =
    (variantTypeName, constructorDeclaration, ~recordGen) => {
  let constructorArgs = constructorDeclaration.Types.cd_args;
  let variantName = Ident.name(constructorDeclaration.Types.cd_id);
  let conversionPlans = reasonTypesToConversion(constructorArgs);
  let convertableFlowTypes =
    conversionPlans |> List.map(({convertableType, _}) => convertableType);
  let dependencies =
    conversionPlans
    |> List.map(({dependencies, _}) => dependencies)
    |> List.concat;
  /* A valid Reason identifier that we can point UpperCase JS exports to. */
  let variantTypeName = variantLeafTypeName(variantTypeName, variantName);
  let (freeTypeVars, remainingDeps) =
    Dependencies.extractFreeTypeVars(dependencies);
  let flowTypeVars = TypeVars.toFlow(freeTypeVars);
  let retType = Ident(variantTypeName, flowTypeVars);
  let constructorFlowType =
    createFunctionFlowType(flowTypeVars, convertableFlowTypes, retType);
  let recordValue =
    recordGen |> Runtime.newRecordValue(~unboxed=constructorArgs == []);
  let codeItems = [
    codeItemForType(
      ~opaque=true,
      flowTypeVars,
      ~typeName=variantTypeName,
      EmitFlow.any,
    ),
    ConstructorBinding(
      constructorFlowType,
      convertableFlowTypes,
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

let codeItemsForId = (~moduleName, ~valueBinding, id) => {
  let {Typedtree.vb_expr, _} = valueBinding;
  let expressionType = vb_expr.exp_type;
  let {dependencies, convertableType: (converter, flowType)} =
    reasonTypeToConversion(expressionType);
  /*
   * We pull apart the polymorphic type variables at the binding level, but
   * not at deeper function types because we know that the Reason/OCaml type
   * system doesn't support higher ranked polymorphism, and so all type
   * variables most likely belong at the binding level.
   */
  let (freeTypeVars, remainingDeps) =
    Dependencies.extractFreeTypeVars(dependencies);
  let flowTypeVars = TypeVars.toFlow(freeTypeVars);
  let flowType = abstractTheTypeParameters(flowType, flowTypeVars);
  let codeItems = [
    ValueBinding(moduleName, Ident.name(id), flowType, converter),
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

let codeItemsForMake = (~moduleName, ~valueBinding, id) => {
  let {Typedtree.vb_expr, _} = valueBinding;
  let expressionType = vb_expr.exp_type;
  let {dependencies, convertableType: (converter, flowType)} =
    reasonTypeToConversion(expressionType);
  let (freeTypeVars, remainingDeps) =
    Dependencies.extractFreeTypeVars(dependencies);
  let flowTypeVars = TypeVars.toFlow(freeTypeVars);
  let flowType = abstractTheTypeParameters(flowType, flowTypeVars);
  switch (flowType) {
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
    let propsTypeArguments =
      switch (childrenOrNil) {
      /* Then we only extracted a function that accepts children, no props */
      | [] => ObjectType([])
      /* Then we had both props and children. */
      | [_children, ..._] =>
        switch (propOrChildren) {
        | ObjectType(fields) =>
          /* Add children?:any to props type */
          let childrenField = ("children", NonMandatory, EmitFlow.any);
          ObjectType(fields @ [childrenField]);
        | _ => propOrChildren
        }
      };
    let propsTypeName = GenIdent.propsTypeName();
    let propsTypeNameFlow = Ident(propsTypeName, []);
    /* TODO: Polymorphic props */
    let componentType = Ident("React$ComponentType", [propsTypeNameFlow]);
    let propsTypeDeclaration = [
      codeItemForType(
        ~opaque=false,
        [],
        ~typeName=propsTypeName,
        propsTypeArguments,
      ),
    ];

    let items =
      propsTypeDeclaration
      @ [
        ComponentBinding({
          moduleName,
          componentType,
          propsTypeName,
          converter,
        }),
      ];
    let deps = [
      JSTypeFromModule(
        "Component",
        Some("ReactComponent"),
        ImportPath.react,
      ),
      ...remainingDeps,
    ];
    (deps, items);

  | _ =>
    /* not a component: treat make as a normal function */
    id |> codeItemsForId(~moduleName, ~valueBinding)
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
let fromValueBinding = (~moduleName, valueBinding) => {
  let {Typedtree.vb_pat, vb_attributes, _} = valueBinding;
  switch (vb_pat.pat_desc, getGenFlowKind(vb_attributes)) {
  | (Tpat_var(id, _), GenFlow) when Ident.name(id) == "make" =>
    id |> codeItemsForMake(~moduleName, ~valueBinding)
  | (Tpat_var(id, _), GenFlow) =>
    id |> codeItemsForId(~moduleName, ~valueBinding)
  | _ => ([], [])
  };
};

/**
 * [@genFlow]
 * [@bs.module] external myBanner : ReasonReact.reactClass = "./MyBanner";
 */
let fromValueDescription = (valueDescription: Typedtree.value_description) => {
  let componentName =
    valueDescription.val_id |> Ident.name |> String.capitalize;
  let path =
    switch (valueDescription.val_prim) {
    | [firstValPrim, ..._] => firstValPrim
    | [] => ""
    };
  let importPath = path |> ImportPath.fromStringUnsafe;
  let conversionPlan =
    valueDescription.val_desc.ctyp_type |> reasonTypeToConversion;
  let flowType = conversionPlan.convertableType |> snd;
  let genFlowKind = getGenFlowKind(valueDescription.val_attributes);
  switch (flowType, genFlowKind) {
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

let fromTypeDecl = (dec: Typedtree.type_declaration) =>
  switch (
    dec.typ_type.type_params,
    dec.typ_type.type_kind,
    getGenFlowKind(dec.typ_attributes),
  ) {
  | (typeParams, Type_record(_, _), GenFlow | GenFlowOpaque) =>
    let freeTypeVars = TypeVars.extract(typeParams);
    let flowTypeVars = TypeVars.toFlow(freeTypeVars);
    let typeName = Ident.name(dec.typ_id);
    (
      [],
      [codeItemForType(~opaque=true, flowTypeVars, ~typeName, EmitFlow.any)],
    );
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
        [
          codeItemForType(~opaque=true, flowTypeVars, ~typeName, EmitFlow.any),
        ],
      )
    | Some(coreType) =>
      let {dependencies, convertableType: (_converter, flowType)} =
        reasonTypeToConversion(coreType.Typedtree.ctyp_type);
      let structureItems = [
        codeItemForType(~opaque=true, flowTypeVars, ~typeName, flowType),
      ];
      let deps = Dependencies.filterFreeTypeVars(freeTypeVars, dependencies);
      (deps, structureItems);
    };
  | (typeParams, Type_variant(constructorDeclarations), GenFlow)
      when !hasSomeGADTLeaf(constructorDeclarations) =>
    let variantTypeName = Ident.name(dec.typ_id);
    let resultTypesDepsAndVariantLeafBindings = {
      let recordGen = Runtime.recordGen();
      List.map(
        constructorDeclaration =>
          codeItemsFromConstructorDeclaration(
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
    let flowTypeVars = TypeVars.toFlow(TypeVars.extract(typeParams));
    let unionType =
      ExportUnionType({
        typeParams: flowTypeVars,
        leafTypes: resultTypes,
        name: variantTypeName,
      });
    (deps, List.append(items, [unionType]));
  | _ => ([], [])
  };

let typePathToImport =
    (~outputFileRelative, ~resolver, ~config as {modulesMap}, typePath) =>
  switch (typePath) {
  | Path.Pident(id) when Ident.name(id) == "list" =>
    ImportAsFrom(
      "list",
      None,
      ModuleName.reasonPervasives
      |> ModuleResolver.importPathForReasonModuleName(
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
        let asTypeName = typePathToFlowName(typePath);
        asTypeName == typeName ? None : Some(asTypeName);
      },
      moduleName
      |> ModuleResolver.importPathForReasonModuleName(
           ~outputFileRelative,
           ~resolver,
           ~modulesMap,
         ),
    );
  };

let importCompare = (i1, i2) =>
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
    (~outputFileRelative, ~resolver, ~config, dependencies): list(t) => {
  let dependencyToImport = dependency =>
    switch (dependency) {
    | TypeAtPath(p) =>
      typePathToImport(~outputFileRelative, ~resolver, ~config, p)
    | JSTypeFromModule(typeName, asTypeName, importPath) =>
      ImportAsFrom(typeName, asTypeName, importPath)
    | FreeTypeVariable(s, _id) =>
      ImportComment("// Warning polymorphic type unhandled:" ++ s)
    /* TODO: Currently unused. Would be useful for injecting dependencies
     * on runtime converters that end up being used. */
    };
  dependencies
  |> List.map(dependencyToImport)
  |> List.sort_uniq(importCompare)
  |> List.map(import => ImportType(import));
};