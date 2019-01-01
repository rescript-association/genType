open GenTypeCommon;

type recordGen;

type recordValue;

type moduleItemGen;

type moduleItem;

let checkMutableObjectField: (~previousName: string, ~name: string) => bool;

let emitModuleItem: moduleItem => string;

let emitRecordAsBlock:
  (~config: config, ~args: list(string), recordValue) => string;

let emitRecordAsInt: (~config: config, recordValue) => string;

let emitVariantGetLabel: string => string;

let emitVariantGetPayload: string => string;

let emitVariantLabel: (~comment: bool=?, string) => string;

let emitVariantWithPayload: (~label: string, string) => string;

let isMutableObjectField: string => bool;

let moduleItemGen: unit => moduleItemGen;

let newModuleItem: moduleItemGen => moduleItem;

let newRecordValue: (~unboxed: bool, recordGen) => recordValue;

let recordGen: unit => recordGen;