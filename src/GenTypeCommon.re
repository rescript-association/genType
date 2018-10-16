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

type module_ =
  | CommonJS
  | ES6;

type importPath =
  | Relative
  | Node;

type config = {
  language,
  module_,
  importPath,
  reasonReactPath: string,
  bsBlockPath: string,
  modulesMap: ModuleNameMap.t(ModuleName.t),
};

let defaultConfig = {
  language: Flow,
  module_: ES6,
  importPath: Relative,
  reasonReactPath: "reason-react/src/ReasonReact.js",
  bsBlockPath: "bs-platform/lib/js/block.js",
  modulesMap: ModuleNameMap.empty,
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
        Filename.concat(projectRoot^, ".genTypeLog"),
      );
    logFile := Some(f);
    f;
  | Some(f) => f
  };

let logItem = x => {
  let outChannel =
    switch (Debug.channel) {
    | Stdout => stdout
    | Logfile => getLogFile()
    };
  Printf.fprintf(outChannel, "  ");
  Printf.fprintf(outChannel, x);
};

type optionalness =
  | NonMandatory
  | Mandatory;

type typ =
  | Ident(string, list(typ))
  | TypeVar(string)
  | Option(typ)
  | Nullable(typ)
  | Array(typ)
  | GroupOfLabeledArgs(fields)
  | Object(fields)
  | Record(fields)
  | Function(function_)
and fields = list((string, optionalness, typ))
and function_ = {
  typeVars: list(string),
  argTypes: list(typ),
  retType: typ,
};

type variant = {
  name: string,
  params: list(typ),
};

type label =
  | Nolabel
  | Label(string)
  | OptLabel(string);

let mixedOrUnknown = (~language) =>
  Ident(
    switch (language) {
    | Flow => "mixed"
    | Typescript
    | Untyped => "unknown"
    },
    [],
  );

let booleanT = Ident("boolean", []);
let numberT = Ident("number", []);
let stringT = Ident("string", []);
let unitT = Ident("void", []);