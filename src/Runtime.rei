open GenTypeCommon;

type recordGen;

type recordValue;

type moduleItemGen;

type moduleItem;

let emitModuleItem: moduleItem => string;

let emitRecordAsBlock:
  (~language: language, ~args: list(string), recordValue) => string;

let emitRecordAsInt: (~language: language, recordValue) => string;

let emitVariantLabel: string => int;

let moduleItemGen: unit => moduleItemGen;

let newModuleItem: moduleItemGen => moduleItem;

let newRecordValue: (~unboxed: bool, recordGen) => recordValue;

let recordGen: unit => recordGen;