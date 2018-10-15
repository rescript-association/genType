open GenTypeCommon;

type recordGen;

type recordValue;

type moduleItemGen;

type moduleItem;

let emitRecordAsInt: (~language: language, recordValue) => string;

let emitRecordAsBlock:
  (~language: language, ~args: list(string), recordValue) => string;

let emitModuleItem: moduleItem => string;

let moduleItemGen: unit => moduleItemGen;

let recordGen: unit => recordGen;

let newRecordValue: (~unboxed: bool, recordGen) => recordValue;

let newModuleItem: moduleItemGen => moduleItem;