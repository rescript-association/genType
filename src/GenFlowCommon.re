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
  | Array(typ)
  | Object(fields)
  | Record(fields)
  | Function(function_)
and fields = list((string, optionalness, typ))
and function_ = {
  typeVars: list(string),
  argTypes: list(typ),
  retType: typ,
};

type label =
  | Nolabel
  | Label(string)
  | OptLabel(string);

let any = Ident("any", []);