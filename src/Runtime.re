open GenTypeCommon;

type recordGen = {
  mutable unboxed: int,
  mutable boxed: int,
};

type recordValue = int;

type moduleItem = string;

type moduleAccessPath =
  | Root(string)
  | Dot(moduleAccessPath, moduleItem);

let recordValueToString = recordValue => recordValue |> string_of_int;

let recordGen = () => {unboxed: 0, boxed: 0};

let newRecordValue = (~unboxed, recordGen) =>
  if (unboxed) {
    let v = recordGen.unboxed;
    recordGen.unboxed = recordGen.unboxed + 1;
    v;
  } else {
    let v = recordGen.boxed;
    recordGen.boxed = recordGen.boxed + 1;
    v;
  };

let newModuleItem = (~name) => {
  name;
};

let rec emitModuleAccessPath = (~config, moduleAccessPath) =>
  switch (moduleAccessPath) {
  | Root(s) => s
  | Dot(p, moduleItem) =>
    p
    |> emitModuleAccessPath(~config)
    |> EmitText.fieldAccess(~label=moduleItem)
  };

let emitVariantLabel = (~polymorphic, label) =>
  if (polymorphic) {
    label |> EmitText.quotes;
  } else {
    label;
  };

module VariantsAsObjects = {
  let polyVariantLabelName = "NAME";
  let label = (~polymorphic) => {
    polymorphic ? polyVariantLabelName : "TAG";
  };

  let indexLabel = i => "_" ++ string_of_int(i);
};

let emitVariantGetLabel = (~polymorphic, x) =>
  x |> EmitText.fieldAccess(~label=VariantsAsObjects.label(~polymorphic));

let accessVariant = (~index, x) => {
  x |> EmitText.fieldAccess(~label=index |> VariantsAsObjects.indexLabel);
};

let emitVariantGetPayload = (~inlineRecord, ~numArgs, ~polymorphic, x) =>
  if (polymorphic) {
    x |> EmitText.fieldAccess(~label="VAL");
  } else if (numArgs == 1) {
    if (inlineRecord) {
      // inline record is repressented as record plus a tag:
      // here pass it unchanged as if it was just a record (the payload)
      x;
    } else {
      x |> accessVariant(~index=0);
    };
  } else {
    /* payload items extracted later when numArgs != 1 */
    x;
  };

let emitVariantWithPayload =
    (~config, ~inlineRecord, ~label, ~polymorphic, args) =>
  switch (args) {
  | [arg] when polymorphic =>
    "{"
    ++ VariantsAsObjects.polyVariantLabelName
    ++ ": "
    ++ (label |> emitVariantLabel(~polymorphic))
    ++ ", VAL: "
    ++ arg
    ++ "}"
  | [arg] when inlineRecord =>
    // inline records are represented as records plus a `TAG`
    "Object.assign({TAG: " ++ label ++ "}, " ++ arg ++ ")"
  | _ =>
    "{TAG: "
    ++ label
    ++ ", "
    ++ (
      args
      |> List.mapi((i, s) => (i |> VariantsAsObjects.indexLabel) ++ ":" ++ s)
      |> String.concat(", ")
    )
    ++ "}"
    ++ (config.language == TypeScript ? " as any" : "")
  };

let jsVariantTag = (~polymorphic) => polymorphic ? "NAME" : "tag";

let jsVariantValue = (~polymorphic) => polymorphic ? "VAL" : "value";

let emitJSVariantGetLabel = (~polymorphic, x) =>
  x |> EmitText.fieldAccess(~label=jsVariantTag(~polymorphic));

let emitJSVariantGetPayload = (~polymorphic, x) =>
  x |> EmitText.fieldAccess(~label=jsVariantValue(~polymorphic));

let emitJSVariantWithPayload = (~label, ~polymorphic, x) => {
  "{"
  ++ jsVariantTag(~polymorphic)
  ++ ":"
  ++ label
  ++ ", "
  ++ jsVariantValue(~polymorphic)
  ++ ":"
  ++ x
  ++ "}";
};

let isMutableObjectField = name =>
  String.length(name) >= 2
  && String.sub(name, String.length(name) - 2, 2) == "#=";

/* Mutable fields, i.e. fields annotated "[@bs.set]"
   are represented as extra fields called "fieldName#="
   preceding the normal field. */
let checkMutableObjectField = (~previousName, ~name) =>
  previousName == name ++ "#=";

let default = "$$default";

module Mangle = {
  let keywords = [|
    "and",
    "as",
    "assert",
    "begin",
    "class",
    "constraint",
    "do",
    "done",
    "downto",
    "else",
    "end",
    "exception",
    "external",
    "false",
    "for",
    "fun",
    "function",
    "functor",
    "if",
    "in",
    "include",
    "inherit",
    "initializer",
    "lazy",
    "let",
    "match",
    "method",
    "module",
    "mutable",
    "new",
    "nonrec",
    "object",
    "of",
    "open",
    "or",
    "private",
    "rec",
    "sig",
    "struct",
    "then",
    "to",
    "true",
    "try",
    "type",
    "val",
    "virtual",
    "when",
    "while",
    "with",
    "mod",
    "land",
    "lor",
    "lxor",
    "lsl",
    "lsr",
    "asr",
  |];

  let table = Hashtbl.create(keywords |> Array.length);
  keywords |> Array.iter(x => Hashtbl.add(table, "_" ++ x, x));

  /*
     Apply bucklescript's mangling rules for object field names:
     Remove trailing "__" if present.
     Otherwise remove leading "_" when followed by an uppercase letter, or keyword.
   */
  let translate = x => {
    let len = x |> String.length;
    if (len > 2 && x.[len - 1] == '_' && x.[len - 2] == '_') {
      /* "foo__" -> "foo" */
      String.sub(x, 0, len - 2);
    } else if (len > 1 && x.[0] == '_') {
      if (x.[1] >= 'A' && x.[1] <= 'Z') {
        /* "_Uppercase" => "Uppercase" */
        String.sub(x, 1, len - 1);
      } else {
        /* "_rec" -> "rec" */
        switch (Hashtbl.find(table, x)) {
        | y => y
        | exception Not_found => x
        };
      };
    } else {
      x;
    };
  };
};

let mangleObjectField = Mangle.translate;
