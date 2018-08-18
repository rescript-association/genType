/***
 * Copyright 2004-present Facebook. All Rights Reserved.
 */

module StringMap = Map.Make(String);
module StringSet = Set.Make(String);
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
  | Ident(string, list(typ))
  | TypeVar(string)
  | Option(typ)
  /* List of typ is the type arguments applied */
  | Object(fields)
  /* List of typ is the type parameters abstracted. Not the arguments
   * applied. */
  | Record(fields)
  | Function(list(string), list(typ), typ)
and fields = list((string, optionalness, typ));

type label =
  | Nolabel
  | Label(string)
  | OptLabel(string);

let any = Ident("any", []);