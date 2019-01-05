open GenTypeCommon;

type recordGen;

type recordValue;

type moduleItemGen;

type moduleItem;

let checkMutableObjectField: (~previousName: string, ~name: string) => bool;

let emitJSVariantGetLabel: string => string;

let emitJSVariantGetPayload: string => string;

let emitJSVariantWithPayload: (~label: string, string) => string;

let emitModuleItem: moduleItem => string;

let emitRecordAsBlock:
  (
    ~config: config,
    ~args: list(string),
    ~useCreateBucklescriptBlock: ref(bool),
    recordValue
  ) =>
  string;

let emitRecordAsInt: (~config: config, recordValue) => string;

let emitVariantGetLabel: (~polyVariant: bool, string) => string;

let emitVariantGetPayload: (~polyVariant: bool, string) => string;

let emitVariantLabel: (~comment: bool=?, ~polyVariant: bool, string) => string;

let emitVariantWithPayload:
  (
    ~label: string,
    ~polyVariant: bool,
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