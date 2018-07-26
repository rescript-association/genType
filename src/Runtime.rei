type recordGen;

type recordValue;

let emitRecordAsInt: recordValue => string;

let emitRecordAsBlock: (~args: list(string), recordValue) => string;

let recordGen: unit => recordGen;

let newRecordValue: (~unboxed: bool, recordGen) => recordValue;