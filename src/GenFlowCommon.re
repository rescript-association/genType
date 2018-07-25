/***
 * Copyright 2004-present Facebook. All Rights Reserved.
 */

module StringMap = Map.Make(String);

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
    cmt |> handleNamespace |> Filename.basename |> ModuleName.fromString;

  let executable =
    Sys.executable_name |> is_relative ?
      concat(Unix.getcwd(), Sys.executable_name) : Sys.executable_name;

  let defaultModulesMap = () => concat(projectRoot^, "modulesMap.txt");
  let absoluteFromProject = filePath =>
    Filename.(
      filePath |> is_relative ? concat(projectRoot^, filePath) : filePath
    );

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
};
let tagSearch = "genFlow";
let tagSearchOpaque = "genFlow.opaque";
let jsTypeNameForAnonymousTypeID = id => "T" ++ string_of_int(id);

type optionalness =
  | NonMandatory
  | Mandatory;

module Flow = {
  /* Introduction of type variables (for all) */
  type typ =
    | Optional(typ)
    /* List of typ is the type arguments applied */
    | Ident(string, list(typ))
    | ObjectType(list((string, optionalness, typ)))
    /* List of typ is the type parameters abstracted. Not the arguments
     * applied. */
    | Arrow(list(typ), list(typ), typ);
  let genericsString = genericStrings =>
    genericStrings === [] ?
      "" : "<" ++ String.concat(",", genericStrings) ++ ">";
  let rec render = typ =>
    switch (typ) {
    | Optional(typ) => "?" ++ render(typ)
    | Ident(identPath, typeArguments) =>
      identPath ++ genericsString(List.map(render, typeArguments))
    | ObjectType(fields) => renderObjType(fields)
    | Arrow(typeParams, valParams, retType) =>
      renderFunType(typeParams, valParams, retType)
    }
  and renderField = ((lbl, optness, typ)) => {
    let optMarker = optness === NonMandatory ? "?" : "";
    lbl ++ optMarker ++ ":" ++ render(typ);
  }
  and renderObjType = fields =>
    "{|" ++ String.concat(", ", List.map(renderField, fields)) ++ "|}"
  /* TODO: Always drop the final unit argument. */
  and renderFunType = (typeParams, valParams, retType) =>
    genericsString(List.map(render, typeParams))
    ++ "("
    ++ String.concat(", ", List.map(render, valParams))
    ++ ") => "
    ++ render(retType);

  /* Applies type parameters to types (for all) */
  let abstractTheTypeParameters = (typ, params) =>
    switch (typ) {
    | Optional(_) => typ
    | Ident(_) => typ
    | ObjectType(_) => typ
    | Arrow(_, valParams, retType) => Arrow(params, valParams, retType)
    };
  /* Assumes post processing will inject an alias to any. */
  let anyAlias = Ident("any", []);
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