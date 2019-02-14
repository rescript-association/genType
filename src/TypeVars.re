open GenTypeCommon;

/**
  * Extracts type variables from dependencies.
  */
let rec extractOne = (~typeVarsGen, soFar, typeExpr) =>
  switch (typeExpr) {
  | {Types.id, desc: Tvar(None), _} =>
    let typeName = GenIdent.jsTypeNameForAnonymousTypeID(~typeVarsGen, id);
    [typeName, ...soFar];
  | {desc: Tvar(Some(s)), _} =>
    let typeName = s;
    [typeName, ...soFar];
  | {desc: Tlink(te), _} => te |> extractOne(~typeVarsGen, soFar)
  | {desc: Tobject(_), _} =>
    let typeName = GenIdent.jsTypeNameForObject(~typeVarsGen);
    [typeName, ...soFar];
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
let toTyp = freeTypeVars => freeTypeVars |> List.map(name => TypeVar(name));

let rec substitute = (~f, typ) =>
  switch (typ) {
  | Array(typ, arrayKind) => Array(typ |> substitute(~f), arrayKind)
  | Function(function_) =>
    Function({
      ...function_,
      argTypes: function_.argTypes |> List.map(substitute(~f)),
      retType: function_.retType |> substitute(~f),
    })
  | GroupOfLabeledArgs(fields) =>
    GroupOfLabeledArgs(
      fields
      |> List.map(field => {...field, typ: field.typ |> substitute(~f)}),
    )
  | Ident(_, []) => typ
  | Ident(name, typeArguments) =>
    Ident(name, typeArguments |> List.map(substitute(~f)))
  | Nullable(typ) => Nullable(typ |> substitute(~f))
  | Object(closedFlag, fields) =>
    Object(
      closedFlag,
      fields
      |> List.map(field => {...field, typ: field.typ |> substitute(~f)}),
    )
  | Option(typ) => Option(typ |> substitute(~f))
  | Record(fields) =>
    Record(
      fields
      |> List.map(field => {...field, typ: field.typ |> substitute(~f)}),
    )
  | Tuple(innerTypes) => Tuple(innerTypes |> List.map(substitute(~f)))
  | TypeVar(s) =>
    switch (f(s)) {
    | None => typ
    | Some(typ1) => typ1
    }
  | Variant(variant) =>
    Variant({
      ...variant,
      payloads:
        variant.payloads
        |> List.map(((case, numArgs, t)) =>
             (case, numArgs, t |> substitute(~f))
           ),
    })
  };

let rec free_ = typ: StringSet.t =>
  switch (typ) {
  | Array(typ, _) => typ |> free_
  | Function({argTypes, retType, typeVars}) =>
    StringSet.diff(
      (argTypes |> freeOfList_) +++ (retType |> free_),
      typeVars |> StringSet.of_list,
    )
  | GroupOfLabeledArgs(fields)
  | Object(_, fields)
  | Record(fields) =>
    fields
    |> List.fold_left(
         (s, {typ, _}) => StringSet.union(s, typ |> free_),
         StringSet.empty,
       )
  | Ident(_, typeArgs) =>
    typeArgs
    |> List.fold_left(
         (s, typeArg) => StringSet.union(s, typeArg |> free_),
         StringSet.empty,
       )
  | Nullable(typ) => typ |> free_
  | Option(typ) => typ |> free_
  | Tuple(innerTypes) =>
    innerTypes
    |> List.fold_left(
         (s, typeArg) => StringSet.union(s, typeArg |> free_),
         StringSet.empty,
       )
  | TypeVar(s) => s |> StringSet.singleton
  | Variant({payloads}) =>
    payloads
    |> List.fold_left(
         (s, (_, _, t)) => StringSet.union(s, t |> free_),
         StringSet.empty,
       )
  }
and freeOfList_ = typs =>
  typs |> List.fold_left((s, t) => s +++ (t |> free_), StringSet.empty)
and (+++) = StringSet.union;

let free = typ => typ |> free_ |> StringSet.elements;
let freeOfList = typ => typ |> freeOfList_ |> StringSet.elements;