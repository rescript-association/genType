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
  bsBlockPath: string,
  bsCurryPath: string,
  importPath,
  inlineAnnotations: bool,
  language,
  module_,
  modulesMap: ModuleNameMap.t(ModuleName.t),
  reasonReactPath: string,
};

let defaultConfig = {
  bsBlockPath: "bs-platform/lib/js/block.js",
  bsCurryPath: "bs-platform/lib/js/curry.js",
  importPath: Relative,
  inlineAnnotations: false,
  language: Flow,
  module_: ES6,
  modulesMap: ModuleNameMap.empty,
  reasonReactPath: "reason-react/src/ReasonReact.js",
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

type optional =
  | Mandatory
  | Optional;

type mutable_ =
  | Immutable
  | Mutable;

type case = {
  label: string,
  labelJS: string,
};

type enum = {
  cases: list(case),
  toJS: string,
  toRE: string,
};

type typ =
  | Array(typ, mutable_)
  | Enum(enum)
  | Function(function_)
  | GroupOfLabeledArgs(fields)
  | Ident(string, list(typ))
  | Nullable(typ)
  | Object(fields)
  | Option(typ)
  | Record(fields)
  | Tuple(list(typ))
  | TypeVar(string)
and fields = list(field)
and field = {
  name: string,
  optional,
  mutable_,
  typ,
}
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

type genTypeKind =
  | Generated
  | GenType
  | GenTypeOpaque
  | NoGenType;

let genTypeKindToString = genTypeKind =>
  switch (genTypeKind) {
  | Generated => "Generated"
  | GenType => "GenType"
  | GenTypeOpaque => "GenTypeOpaque"
  | NoGenType => "NoGenType"
  };

let createEnum = cases => {
  let hash =
    cases
    |> List.map(case => (case.label, case.labelJS))
    |> Array.of_list
    |> Hashtbl.hash
    |> string_of_int;
  Enum({cases, toJS: "$$toJS" ++ hash, toRE: "$$toRE" ++ hash});
};

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