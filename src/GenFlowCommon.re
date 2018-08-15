/***
 * Copyright 2004-present Facebook. All Rights Reserved.
 */

module StringMap = Map.Make(String);
module ModuleNameMap = Map.Make(ModuleName);

type language =
  | Flow
  | Typescript
  | Untyped;

type config = {
  language,
  modulesMap: ModuleNameMap.t(ModuleName.t),
};

let log = Printf.printf;
let logItem = x => {
  log("  ");
  log(x);
};

let tagSearch = "genFlow";
let tagSearchOpaque = "genFlow.opaque";

type optionalness =
  | NonMandatory
  | Mandatory;

type typ =
  | Optional(typ)
  /* List of typ is the type arguments applied */
  | Ident(string, list(typ))
  | ObjectType(fields)
  /* List of typ is the type parameters abstracted. Not the arguments
   * applied. */
  | Arrow(list(typ), list(typ), typ)
and fields = list((string, optionalness, typ));

type label =
  | Nolabel
  | Label(string)
  | OptLabel(string);

type converter =
  | Unit
  | Identity
  | OptionalArgument(converter)
  | Option(converter)
  | Fn((list(groupedArgConverter), converter))
and groupedArgConverter =
  | ArgConverter(label, converter)
  | GroupConverter(list((string, converter)));

type convertableType = (converter, typ);

type groupedArg =
  /* Contains a list of (name, isOptional, 't)  */
  | Group(list((string, optionalness, convertableType)))
  | Arg(convertableType);

let any = Ident("any", []);

let rec subTypeVars = (~f, typ) =>
  switch (typ) {
  | Optional(typ) => Optional(typ |> subTypeVars(~f))
  | Ident(s, []) =>
    switch (f(s)) {
    | None => typ
    | Some(typ') => typ'
    }
  | Ident(s, [_, ..._]) => typ
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