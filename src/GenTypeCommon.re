/***
 * Copyright 2004-present Facebook. All Rights Reserved.
 */

module StringMap = Map.Make(String);
module StringSet = Set.Make(String);

module Config = Config_;
include Config;

let logNotImplemented = x =>
  if (Debug.notImplemented^) {
    logItem("Not Implemented: %s\n", x);
  };

type optional =
  | Mandatory
  | Optional;

type mutable_ =
  | Immutable
  | Mutable;

type labelJS =
  | BoolLabel(bool)
  | FloatLabel(string)
  | IntLabel(int)
  | StringLabel(string);

let labelJSToString = (~alwaysQuotes=false, labelJS) => {
  let addQuotes = x => alwaysQuotes ? x |> EmitText.quotes : x;
  switch (labelJS) {
  | BoolLabel(b) => b |> string_of_bool |> addQuotes
  | FloatLabel(s) => s |> addQuotes
  | IntLabel(i) => i |> string_of_int |> addQuotes
  | StringLabel(s) => s |> EmitText.quotes
  };
};

type case = {
  label: string,
  labelJS,
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
  | Variant(list(variantLeaf))
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
}
and variantLeaf = {
  leafName: string,
  argTypes: list(typ),
}
and enum = {
  cases: list(case),
  withPayload: list((case, int, typ)),
  polyVariant: bool,
  toJS: string,
  toRE: string,
  unboxed: bool,
};

let typIsObject = typ =>
  switch (typ) {
  | Array(_) => true
  | Enum(_) => false
  | Function(_) => false
  | GroupOfLabeledArgs(_) => false
  | Ident(_) => false
  | Nullable(_) => false
  | Object(_) => true
  | Option(_) => false
  | Record(_) => true
  | Tuple(_) => true
  | TypeVar(_) => false
  | Variant(_) => false
  };

type variant = {
  name: string,
  params: list(typ),
};

type label =
  | Nolabel
  | Label(string)
  | OptLabel(string);

let createEnum = (~withPayload, ~polyVariant, cases) => {
  let hash =
    cases
    |> List.map(case => (case.label, case.labelJS))
    |> Array.of_list
    |> Hashtbl.hash
    |> string_of_int;
  let unboxed = withPayload == [];
  Enum({
    cases,
    withPayload,
    polyVariant,
    toJS: "$$toJS" ++ hash,
    toRE: "$$toRE" ++ hash,
    unboxed,
  });
};

let mixedOrUnknown = (~config) =>
  Ident(
    switch (config.language) {
    | Flow => "mixed"
    | TypeScript
    | Untyped => "unknown"
    },
    [],
  );

let booleanT = Ident("boolean", []);
let numberT = Ident("number", []);
let stringT = Ident("string", []);
let unitT = Ident("void", []);

module NodeFilename = {
  include Filename;

  /* Force "/" separator. */
  let dirSep = "/";

  module Path: {
    type t;
    let normalize: string => t;
    let concat: (t, string) => t;
    let length: t => int;
    let toString: t => string;
  } = {
    type t = string;

    let normalize = path: t =>
      switch (Sys.os_type) {
      | "Win32" =>
        path |> Str.split(Str.regexp("\\")) |> String.concat(dirSep)
      | _ => path
      };

    let toString = path => path;
    let length = path => String.length(path);

    let concat = (dirname, filename) => {
      let isDirSep = (s, i) => {
        let c = s.[i];
        c == '/' || c == '\\' || c == ':';
      };
      let l = length(dirname);
      if (l == 0 || isDirSep(dirname, l - 1)) {
        dirname ++ filename;
      } else {
        dirname ++ dirSep ++ filename;
      };
    };
  };

  let concat = (dirname: string, filename) =>
    Path.(Path.concat(normalize(dirname), filename) |> toString);
};