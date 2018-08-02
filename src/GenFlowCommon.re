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