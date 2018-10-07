open GenTypeCommon;

/**
  * Extracts type variables from dependencies.
  */
let extractOne = (~typeVarsGen, soFar, typ) =>
  switch (typ) {
  | {Types.id, desc: Tvar(None), _} =>
    let typeName = GenIdent.jsTypeNameForAnonymousTypeID(~typeVarsGen, id);
    [typeName, ...soFar];
  | {desc: Tvar(Some(s)), _} =>
    let typeName = s;
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
  | Ident(_, []) => typ
  | Ident(name, typeArguments) =>
    Ident(name, typeArguments |> List.map(substitute(~f)))
  | TypeVar(s) =>
    switch (f(s)) {
    | None => typ
    | Some(typ') => typ'
    }
  | Option(typ) => Option(typ |> substitute(~f))
  | Nullable(typ) => Nullable(typ |> substitute(~f))
  | Array(typ) => Array(typ |> substitute(~f))
  | GroupOfLabeledArgs(fields) =>
    GroupOfLabeledArgs(
      fields
      |> List.map(((s, optionalness, t)) =>
           (s, optionalness, t |> substitute(~f))
         ),
    )
  | Object(fields) =>
    Object(
      fields
      |> List.map(((s, optionalness, t)) =>
           (s, optionalness, t |> substitute(~f))
         ),
    )
  | Record(fields) =>
    Record(
      fields
      |> List.map(((s, optionalness, t)) =>
           (s, optionalness, t |> substitute(~f))
         ),
    )
  | Function(function_) =>
    Function({
      ...function_,
      argTypes: function_.argTypes |> List.map(substitute(~f)),
      retType: function_.retType |> substitute(~f),
    })
  };

let rec free_ = typ: StringSet.t =>
  switch (typ) {
  | Ident(_, typeArgs) =>
    typeArgs
    |> List.fold_left(
         (s, typeArg) => StringSet.union(s, typeArg |> free_),
         StringSet.empty,
       )
  | TypeVar(s) => s |> StringSet.singleton
  | Option(typ) => typ |> free_
  | Nullable(typ) => typ |> free_
  | Array(typ) => typ |> free_
  | GroupOfLabeledArgs(fields)
  | Object(fields)
  | Record(fields) =>
    fields
    |> List.fold_left(
         (s, (_, _, t)) => StringSet.union(s, t |> free_),
         StringSet.empty,
       )
  | Function({typeVars, argTypes, retType}) =>
    StringSet.diff(
      (argTypes |> freeOfList_) +++ (retType |> free_),
      typeVars |> StringSet.of_list,
    )
  }
and freeOfList_ = typs =>
  typs |> List.fold_left((s, t) => s +++ (t |> free_), StringSet.empty)
and (+++) = StringSet.union;

let free = typ => typ |> free_ |> StringSet.elements;
let freeOfList = typ => typ |> freeOfList_ |> StringSet.elements;