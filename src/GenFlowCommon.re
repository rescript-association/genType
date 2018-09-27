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

let projectRoot = ref("");

let logFile = ref(None);

let getLogFile = () =>
  switch (logFile^) {
  | None =>
    let f =
      open_out_gen(
        [Open_creat, Open_text, Open_append],
        0o640,
        Filename.concat(projectRoot^, ".genFlowLog"),
      );
    logFile := Some(f);
    f;
  | Some(f) => f
  };

let logItem = x => {
  let outChannel = getLogFile();
  Printf.fprintf(outChannel, "  ");
  Printf.fprintf(outChannel, x);
};

let tagIsGenType = s => s == "genFlow" || s == "genType";
let tagIsGenTypeAs = s => s == "genFlow" || s == "genType" || s == "genFlow.as" || s == "genType.as";
let tagIsGenFlowOpaque = s => s == "genFlow.opaque";

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