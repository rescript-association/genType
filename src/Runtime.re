let createBucklescriptBlock = "CreateBucklescriptBlock" ++ ".__";

type recordGen = {
  mutable unboxed: int,
  mutable boxed: int,
};

type recordValue = int;

let emitRecordAsInt = (~config, i) => i |> EmitTyp.blockTagValue(~config);

let emitRecordAsBlock = (~config, ~args, recordValue) =>
  createBucklescriptBlock
  |> Emit.funCall(
       ~args=[recordValue |> emitRecordAsInt(~config), Emit.array(args)],
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