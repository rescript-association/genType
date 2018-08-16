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

let rec subTypeVars = (~f, typ) =>
  switch (typ) {
  | Optional(typ) => Optional(typ |> subTypeVars(~f))
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
           (s, optionalness, t |> subTypeVars(~f))
         ),
    )
  | Arrow(typs1, typs2, t) =>
    Arrow(
      typs1 |> List.map(subTypeVars(~f)),
      typs2 |> List.map(subTypeVars(~f)),
      t |> subTypeVars(~f),
    )
  };

let rec freeTypeVars = typ: StringSet.t =>
  switch (typ) {
  | Optional(typ) => typ |> freeTypeVars
  | TypeVar(s) => s |> StringSet.singleton
  | Ident(_) => StringSet.empty
  | ObjectType(fields) =>
    fields
    |> List.fold_left(
         (s, (_, _, t)) => StringSet.union(s, t |> freeTypeVars),
         StringSet.empty,
       )
  | Arrow(typs1, typs2, t) =>
    (typs1 |> freeTypeVarsOfList)
    +++ (typs2 |> freeTypeVarsOfList)
    +++ (t |> freeTypeVars)
  }
and freeTypeVarsOfList = typs =>
  typs
  |> List.fold_left((s, t) => s +++ (t |> freeTypeVars), StringSet.empty)
and (+++) = StringSet.union;