open GenTypeCommon;

let createBucklescriptBlock = "CreateBucklescriptBlock" ++ ".__";

type recordGen = {
  mutable unboxed: int,
  mutable boxed: int,
};

type recordValue = int;

type moduleItemGen = {mutable itemIndex: int};

type moduleItem = {
  name: string,
  index: int,
};

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

let moduleItemGen = () => {itemIndex: 0};

let newModuleItem = (~name, moduleItemGen) => {
  let index = moduleItemGen.itemIndex;
  moduleItemGen.itemIndex = moduleItemGen.itemIndex + 1;
  {name, index};
};

let rec emitModuleAccessPath = (~config, moduleAccessPath) =>
  switch (moduleAccessPath) {
  | Root(s) => s
  | Dot(p, moduleItem) =>
    p
    |> emitModuleAccessPath(~config)
    |> (
      config.modulesAsObjects
        ? EmitText.fieldAccess(~label=moduleItem.name)
        : EmitText.arrayAccess(~index=moduleItem.index)
    )
  };

let emitVariantLabel = (~comment=true, ~config, ~polymorphic, label) =>
  if (polymorphic) {
    config.variantHashesAsStrings
      ? label |> EmitText.quotes
      : (comment ? label |> EmitText.comment : "")
        ++ (label |> Btype.hash_variant |> string_of_int);
  } else {
    label;
  };

module VariantsAsObjects = {
  let polyVariantLabelName = (~config) =>
    config.variantHashesAsStrings ? "NAME" : "HASH";
  let label = (~config, ~polymorphic) => {
    polymorphic ? polyVariantLabelName(~config) : "TAG";
  };

  let indexLabel = i => "_" ++ string_of_int(i);
};

let emitVariantGetLabel = (~config, ~polymorphic, x) =>
  config.variantsAsObjects
    ? x
      |> EmitText.fieldAccess(
           ~label=VariantsAsObjects.label(~config, ~polymorphic),
         )
    : (
      if (polymorphic) {
        x |> EmitText.arrayAccess(~index=0);
      } else {
        x |> EmitText.fieldAccess(~label="tag");
      }
    );

let accessVariant = (~config, ~index, x) =>
  if (config.variantsAsObjects) {
    x |> EmitText.fieldAccess(~label=index |> VariantsAsObjects.indexLabel);
  } else {
    x |> EmitText.arrayAccess(~index);
  };

let emitVariantGetPayload = (~config, ~numArgs, ~polymorphic, x) =>
  if (polymorphic) {
    config.variantsAsObjects
      ? x |> EmitText.fieldAccess(~label="VAL")
      : x |> EmitText.arrayAccess(~index=1);
  } else if (numArgs == 1) {
    x |> accessVariant(~config, ~index=0);
  } else if (numArgs == 0) {
    /* inline record */
    x;
  } else {
    /* to convert a runtime block to a tuple, remove the tag */
    config.variantsAsObjects
      ? x : x |> EmitText.arraySlice;
  };

let emitVariantWithPayload = (~config, ~label, ~numArgs, ~polymorphic, args) =>
  switch (args) {
  | [arg] when polymorphic && config.variantsAsObjects =>
    "{"
    ++ VariantsAsObjects.polyVariantLabelName(~config)
    ++ ": "
    ++ (label |> emitVariantLabel(~config, ~polymorphic))
    ++ ", VAL: "
    ++ arg
    ++ "}"
  | [arg] when polymorphic && !config.variantsAsObjects =>
    [label |> emitVariantLabel(~config, ~polymorphic), arg] |> EmitText.array
  | [arg] when numArgs == 0 =>
    /* inline record */
    arg
  | _ =>
    if (config.variantsAsObjects) {
      "{TAG: "
      ++ label
      ++ ", "
      ++ (
        args
        |> List.mapi((i, s) =>
             (i |> VariantsAsObjects.indexLabel) ++ ":" ++ s
           )
        |> String.concat(", ")
      )
      ++ "}"
      ++ (config.language == TypeScript ? " as any" : "");
    } else {
      config.emitCreateBucklescriptBlock = true;
      createBucklescriptBlock
      |> EmitText.funCall(~args=[label, args |> EmitText.array]);
    }
  };

let jsVariantTag = (~config, ~polymorphic) =>
  polymorphic && config.variantHashesAsStrings ? "NAME" : "tag";

let jsVariantValue = (~config, ~polymorphic) =>
  polymorphic && config.variantHashesAsStrings ? "VAL" : "value";

let emitJSVariantGetLabel = (~config, ~polymorphic, x) =>
  x |> EmitText.fieldAccess(~label=jsVariantTag(~config, ~polymorphic));

let emitJSVariantGetPayload = (~config, ~polymorphic, x) =>
  x |> EmitText.fieldAccess(~label=jsVariantValue(~config, ~polymorphic));

let emitJSVariantWithPayload = (~config, ~label, ~polymorphic, x) => {
  "{"
  ++ jsVariantTag(~config, ~polymorphic)
  ++ ":"
  ++ label
  ++ ", "
  ++ jsVariantValue(~config, ~polymorphic)
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
