let createBucklescriptBlock = "CreateBucklescriptBlock" ++ ".__";

type recordGen = {
  mutable unboxed: int,
  mutable boxed: int,
};

type recordValue = int;

type moduleItemGen = {mutable itemValue: int};

type moduleItem = int;

let emitRecordAsInt = (~language, i) =>
  i |> EmitTyp.blockTagValue(~language);

let emitRecordAsBlock = (~language, ~args, recordValue) =>
  createBucklescriptBlock
  |> EmitText.funCall(
       ~args=[
         recordValue |> emitRecordAsInt(~language),
         EmitText.array(args),
       ],
     );

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

let moduleItemGen = () => {itemValue: 0};

let newModuleItem = moduleItemGen => {
  let v = moduleItemGen.itemValue;
  moduleItemGen.itemValue = moduleItemGen.itemValue + 1;
  v;
};

let emitModuleItem = itemValue => itemValue |> string_of_int;

let emitVariantLabel = (~comment=true, label) =>
  (comment ? label |> EmitText.comment : "")
  ++ (label |> Btype.hash_variant |> string_of_int);

/* Mutable fields, i.e. fields annotated "[@bs.set]"
   are represented as extra fields called "fieldName#="
   preceding the normal field. */
let checkMutableObjectField = (~previousName, ~name) =>
  previousName == name ++ "#=";