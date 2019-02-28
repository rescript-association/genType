open GenTypeCommon;

type recordGen;

type recordValue;

type moduleItemGen;

type moduleItem;

let checkMutableObjectField: (~previousName: string, ~name: string) => bool;

let emitJSVariantGetLabel: Value.t => Value.t;

let emitJSVariantGetPayload: Value.t => Value.t;

let emitJSVariantWithPayload: (~label: string, string) => string;

let emitModuleItem: moduleItem => string;

let emitRecordAsInt: (~config: config, recordValue) => string;

let emitVariantGetLabel: (~polymorphic: bool, Value.t) => Value.t;

let emitVariantGetPayload:
  (~numArgs: int, ~polymorphic: bool, Value.t) => Value.t;

let emitVariantLabel: (~comment: bool=?, ~polymorphic: bool, string) => string;

let emitVariantWithPayload:
  (
    ~label: string,
    ~numArgs: int,
    ~polymorphic: bool,
    ~useCreateBucklescriptBlock: ref(bool),
    string
  ) =>
  string;

let isMutableObjectField: string => bool;

let moduleItemGen: unit => moduleItemGen;

let newModuleItem: moduleItemGen => moduleItem;

let newRecordValue: (~unboxed: bool, recordGen) => recordValue;

let recordGen: unit => recordGen;

let recordValueToString: recordValue => string;

let jsVariantTag: string;

let jsVariantValue: string;