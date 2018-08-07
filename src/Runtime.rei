open GenFlowCommon;

type recordGen;

type recordValue;

let emitRecordAsInt: (~language: language, recordValue) => string;

let emitRecordAsBlock:
  (~language: language, ~args: list(string), recordValue) => string;

let recordGen: unit => recordGen;

let newRecordValue: (~unboxed: bool, recordGen) => recordValue;