/***
 * Copyright 2004-present Facebook. All Rights Reserved.
 */

module StringMap = Map.Make(String);
module ModuleNameMap = Map.Make(ModuleName);

type config = {
  language: string,
  modulesMap: ModuleNameMap.t(ModuleName.t),
};

let log = Printf.printf;
let logItem = x => {
  log("  ");
  log(x);
};

module Debug = {
  let codeItems = false;
  let config = false;
  let moduleResolution = false;
};

module Paths = {
  open Filename;

  let outputFileSuffix = ".re.js";

  let projectRoot = ref(Sys.getcwd());

  let setProjectRoot = s =>
    projectRoot :=
      Filename.is_relative(s) ? Filename.concat(Unix.getcwd(), s) : s;

  /*
   * Handle namespaces in cmt files.
   * E.g. src/Module-Project.cmt becomes src/Module
   */
  let handleNamespace = cmt => {
    let cutAfterDash = s =>
      switch (String.index(s, '-')) {
      | n => String.sub(s, 0, n)
      | exception Not_found => s
      };
    let noDir = Filename.basename(cmt) == cmt;
    if (noDir) {
      cmt |> Filename.chop_extension |> cutAfterDash;
    } else {
      let dir = cmt |> Filename.dirname;
      let base =
        cmt |> Filename.basename |> Filename.chop_extension |> cutAfterDash;
      Filename.concat(dir, base);
    };
  };

  /* Get the output file to be written, relative to the project root. */
  let getOutputFileRelative = cmt =>
    (cmt |> handleNamespace) ++ outputFileSuffix;

  /* Get the output file to be written, as an absolute path. */
  let getOutputFile = cmt =>
    Filename.concat(projectRoot^, getOutputFileRelative(cmt));

  let getModuleName = cmt =>
    cmt |> handleNamespace |> Filename.basename |> ModuleName.fromStringUnsafe;

  let executable =
    Sys.executable_name |> is_relative ?
      concat(Unix.getcwd(), Sys.executable_name) : Sys.executable_name;

  let getConfigFile = () => {
    let fname = concat(projectRoot^, "genflowconfig.json");
    fname |> Sys.file_exists ? Some(fname) : None;
  };

  /* Find the relative path from /.../bs/lib
     e.g. /foo/bar/bs/lib/src/Hello.re --> src/Hello.re */
  let relativePathFromBsLib = fileName =>
    if (is_relative(fileName)) {
      fileName;
    } else {
      let rec pathToList = path => {
        let isRoot = path |> basename == path;
        isRoot ?
          [path] : [path |> basename, ...path |> dirname |> pathToList];
      };
      let rec fromLibBs = (~acc, reversedList) =>
        switch (reversedList) {
        | ["bs", "lib", ..._] => acc
        | [dir, ...dirs] => fromLibBs(~acc=[dir, ...acc], dirs)
        | [] => [] /* not found */
        };
      fileName
      |> pathToList
      |> fromLibBs(~acc=[])
      |> (
        l =>
          switch (l) {
          | [] => fileName
          | [root, ...dirs] => dirs |> List.fold_left(concat, root)
          }
      );
    };

  let concat = concat;

  let getLanguage = json =>
    switch (json) {
    | Ext_json_types.Obj({map, _}) =>
      switch (map |> String_map.find_opt("language")) {
      | Some(Str({str, _})) => str
      | _ => ""
      }
    | _ => ""
    };

  let getShims = json => {
    let shims = ref([]);
    switch (json) {
    | Ext_json_types.Obj({map, _}) =>
      switch (map |> String_map.find_opt("shims")) {
      | Some(Arr({content, _})) =>
        content
        |> Array.iter(x =>
             switch (x) {
             | Ext_json_types.Str({str, _}) => shims := [str, ...shims^]
             | _ => ()
             }
           );
        ();
      | _ => ()
      }
    | _ => ()
    };
    shims^;
  };

  let emptyConfig = {language: "", modulesMap: ModuleNameMap.empty};
  let readConfig = () => {
    let fromJson = json => {
      let language = json |> getLanguage;
      let modulesMap =
        json
        |> getShims
        |> List.fold_left(
             (map, nextPairStr) =>
               if (nextPairStr != "") {
                 let fromTo =
                   Str.split(Str.regexp("="), nextPairStr) |> Array.of_list;
                 assert(Array.length(fromTo) === 2);
                 let moduleName: ModuleName.t =
                   fromTo[0] |> ModuleName.fromStringUnsafe;
                 let shimModuleName = fromTo[1] |> ModuleName.fromStringUnsafe;
                 ModuleNameMap.add(moduleName, shimModuleName, map);
               } else {
                 map;
               },
             ModuleNameMap.empty,
           );
      if (Debug.config) {
        logItem(
          "Config language:%s modulesMap:%d entries\n",
          language,
          modulesMap |> ModuleNameMap.cardinal,
        );
      };
      {language, modulesMap};
    };

    switch (getConfigFile()) {
    | None => emptyConfig
    | Some(configFile) =>
      try (configFile |> Ext_json_parse.parse_json_from_file |> fromJson) {
      | _ => emptyConfig
      }
    };
  };
};

let tagSearch = "genFlow";
let tagSearchOpaque = "genFlow.opaque";
let jsTypeNameForAnonymousTypeID = id => "T" ++ string_of_int(id);

type optionalness =
  | NonMandatory
  | Mandatory;

module Flow = {
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
    | GroupConverter(list((string, optionalness, converter)));

  type convertableType = (converter, typ);

  type groupedArg =
    /* Contains a list of (name, isOptional, 't)  */
    | Group(list((string, optionalness, convertableType)))
    | Arg(convertableType);

  let genericsString = genericStrings =>
    genericStrings === [] ?
      "" : "<" ++ String.concat(",", genericStrings) ++ ">";

  let rec toString = typ =>
    switch (typ) {
    | Optional(typ) => "?" ++ toString(typ)
    | Ident(identPath, typeArguments) =>
      identPath ++ genericsString(List.map(toString, typeArguments))
    | ObjectType(fields) => renderObjType(fields)
    | Arrow(typeParams, valParams, retType) =>
      renderFunType(typeParams, valParams, retType)
    }
  and renderField = ((lbl, optness, typ)) => {
    let optMarker = optness === NonMandatory ? "?" : "";
    lbl ++ optMarker ++ ":" ++ toString(typ);
  }
  and renderObjType = fields =>
    "{|" ++ String.concat(", ", List.map(renderField, fields)) ++ "|}"
  /* TODO: Always drop the final unit argument. */
  and renderFunType = (typeParams, valParams, retType) =>
    genericsString(List.map(toString, typeParams))
    ++ "("
    ++ String.concat(", ", List.map(toString, valParams))
    ++ ") => "
    ++ toString(retType);

  /* Applies type parameters to types (for all) */
  let abstractTheTypeParameters = (typ, params) =>
    switch (typ) {
    | Optional(_) => typ
    | Ident(_) => typ
    | ObjectType(_) => typ
    | Arrow(_, valParams, retType) => Arrow(params, valParams, retType)
    };

  let any = Ident("any", []);
};

/* Generate fresh identifiers */
module GenIdent = {
  /*
   * Keep a few banks of identifiers to make them more readable.
   */

  let propsTypeNameCount = {contents: 0};

  let resetPerFile = () => propsTypeNameCount.contents = 0;

  let propsTypeName = () => {
    propsTypeNameCount.contents = propsTypeNameCount.contents + 1;
    "Props"
    ++ (
      propsTypeNameCount.contents == 1 ?
        "" : string_of_int(propsTypeNameCount.contents)
    );
  };
};

let readLines = (file: string): list(string) => {
  let lines = ref([]);
  let chan = open_in(file);
  let finished_lines =
    try (
      {
        while (true) {
          lines := [input_line(chan), ...lines^];
        };
        [];
      }
    ) {
    | End_of_file =>
      close_in(chan);
      lines^ |> List.rev;
    };
  finished_lines;
};

let readFile = (file: string): string =>
  String.concat("\n", readLines(file));