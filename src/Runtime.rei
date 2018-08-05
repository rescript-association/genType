open GenFlowCommon;

type recordGen;

type recordValue;

let emitRecordAsInt: (~config: config, recordValue) => string;

let emitRecordAsBlock:
  (~config: config, ~args: list(string), recordValue) => string;

let recordGen: unit => recordGen;

let newRecordValue: (~unboxed: bool, recordGen) => recordValue;