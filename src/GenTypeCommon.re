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

type closedFlag =
  | Open
  | Closed;

type type_ =
  | Array(type_, mutable_)
  | Function(function_)
  | GroupOfLabeledArgs(fields)
  | Ident(ident)
  | Null(type_)
  | Nullable(type_)
  | Object(closedFlag, fields)
  | Option(type_)
  | Promise(type_)
  | Record(fields)
  | Tuple(list(type_))
  | TypeVar(string)
  | Variant(variant)
and builtin =
  | Promise(type_)
and fields = list(field)
and field = {
  mutable_,
  name: string,
  optional,
  type_,
}
and function_ = {
  argTypes: list(type_),
  componentName: option(string),
  retType: type_,
  typeVars: list(string),
  uncurried: bool,
}
and ident = {
  builtin: bool,
  name: string,
  typeArgs: list(type_),
}
and variant = {
  hash: int,
  noPayloads: list(case),
  payloads: list((case, int, type_)),
  polymorphic: bool,
  unboxed: bool,
};

let typeIsObject = type_ =>
  switch (type_) {
  | Array(_) => true
  | Function(_) => false
  | GroupOfLabeledArgs(_) => false
  | Ident(_) => false
  | Null(_) => false
  | Nullable(_) => false
  | Object(_) => true
  | Option(_) => false
  | Promise(_) => true
  | Record(_) => true
  | Tuple(_) => true
  | TypeVar(_) => false
  | Variant(_) => false
  };

type label =
  | Nolabel
  | Label(string)
  | OptLabel(string);

type dep =
  | External(string)
  | Internal(ResolvedName.t)
  | Dot(dep, string);

let rec depToString = dep =>
  switch (dep) {
  | External(name) => name
  | Internal(resolvedName) => resolvedName |> ResolvedName.toString
  | Dot(d, s) => depToString(d) ++ "_" ++ s
  };

let rec depToResolvedName = (dep: dep) =>
  switch (dep) {
  | External(name) => name |> ResolvedName.fromString
  | Internal(resolvedName) => resolvedName
  | Dot(p, s) => ResolvedName.dot(s, p |> depToResolvedName)
  };

let createVariant = (~noPayloads, ~payloads, ~polymorphic) => {
  let hash =
    noPayloads
    |> List.map(case => (case.label, case.labelJS))
    |> Array.of_list
    |> Hashtbl.hash;

  let unboxed = payloads == [];
  Variant({hash, noPayloads, payloads, polymorphic, unboxed});
};

let variantTable = (hash, ~toJS) =>
  (toJS ? "$$toJS" : "$$toRE") ++ string_of_int(hash);

let ident = (~builtin=true, ~typeArgs=[], name) =>
  Ident({builtin, name, typeArgs});

let mixedOrUnknown = (~config) =>
  ident(
    switch (config.language) {
    | Flow => "mixed"
    | TypeScript
    | Untyped => "unknown"
    },
  );

let booleanT = ident("boolean");
let numberT = ident("number");
let stringT = ident("string");
let unitT = ident("void");

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