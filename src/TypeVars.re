open GenFlowCommon;

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
  | Optional(typ) => Optional(typ |> substitute(~f))
  | TypeVar(s) =>
    switch (f(s)) {
    | None => typ
    | Some(typ') => typ'
    }
  | Ident(_) => typ
  | ObjectType(fields) =>
    ObjectType(
      fields
      |> List.map(((s, optionalness, t)) =>
           (s, optionalness, t |> substitute(~f))
         ),
    )
  | Arrow(generics, args, t) =>
    Arrow(generics, args |> List.map(substitute(~f)), t |> substitute(~f))
  };

let rec free_ = typ: StringSet.t =>
  switch (typ) {
  | Optional(typ) => typ |> free_
  | TypeVar(s) => s |> StringSet.singleton
  | Ident(_) => StringSet.empty
  | ObjectType(fields) =>
    fields
    |> List.fold_left(
         (s, (_, _, t)) => StringSet.union(s, t |> free_),
         StringSet.empty,
       )
  | Arrow(generics, args, t) =>
    StringSet.diff(
      (args |> freeOfList_) +++ (t |> free_),
      generics |> StringSet.of_list,
    )
  }
and freeOfList_ = typs =>
  typs |> List.fold_left((s, t) => s +++ (t |> free_), StringSet.empty)
and (+++) = StringSet.union;

let free = typ => typ |> free_ |> StringSet.elements;
let freeOfList = typ => typ |> freeOfList_ |> StringSet.elements;