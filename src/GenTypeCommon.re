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

let createEnum = cases => {
  let hash =
    cases
    |> List.map(case => (case.label, case.labelJS))
    |> Array.of_list
    |> Hashtbl.hash
    |> string_of_int;
  Enum({cases, toJS: "$$toJS" ++ hash, toRE: "$$toRE" ++ hash});
};

let mixedOrUnknown = (~config) =>
  Ident(
    switch (config.language) {
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


module NodeFilename {
  include Filename;

  /* Force "/" separator. */
  let dir_sep = "/";

  module Path: {  
    type t;
    let normalize: string => t;
    let concat: (t, string) => t;
    let length: t => int;
    let toString: t => string;
  } = {
    type t = string;

    let normalize = (path): t => {
      switch (Sys.os_type) {
      | "Win32" => {
          let exp = Str.regexp("\\");
          List.fold_left((acc, next) => {
            switch(acc) {
              | "" => next
              | _ => acc ++ dir_sep ++ next
            }
          }, "", Str.split(exp, path));
        }
      | _ => path
      }
    }

    let toString = path => path;
    let length = path => String.length(path);

    let concat = (dirname, filename) => {
      let is_dir_sep = (s, i) => {
        let c = s.[i];
        c == '/' || c == '\\' || c == ':';
      };
      let l = length(dirname);
      if (l == 0 || is_dir_sep(dirname, l - 1)) {
        dirname ++ filename;
      } else {
        dirname ++ dir_sep ++ filename;
      };
    }
  };
  
  let concat = (dirname: string, filename) => {
    open Path;
    Path.concat(normalize(dirname), filename) |> toString
  }
};