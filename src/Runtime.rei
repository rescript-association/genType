open GenTypeCommon;

type recordGen;

type recordValue;

type moduleItemGen;

type moduleItem;

type moduleAccessPath =
  | Root(string)
  | Dot(moduleAccessPath, moduleItem);

let checkMutableObjectField: (~previousName: string, ~name: string) => bool;

/* Internal name of a value called "default" used by buclescript for default export */
let default: string;

let emitModuleAccessPath: (~config: config, moduleAccessPath) => string;

let emitJSVariantGetLabel:
  (~config: config, ~polymorphic: bool, string) => string;

let emitJSVariantGetPayload:
  (~config: config, ~polymorphic: bool, string) => string;

let emitJSVariantWithPayload:
  (
    ~config: config,
    ~label: string,
    ~numArgs: int,
    ~polymorphic: bool,
    string
  ) =>
  string;

let emitVariantGetLabel:
  (~config: config, ~polymorphic: bool, string) => string;

let emitVariantGetPayload:
  (~config: config, ~numArgs: int, ~polymorphic: bool, string) => string;

let emitVariantLabel:
  (~comment: bool=?, ~config: config, ~polymorphic: bool, string) => string;

let emitVariantWithPayload:
  (
    ~config: config,
    ~label: string,
    ~numArgs: int,
    ~polymorphic: bool,
    string
  ) =>
  string;

let isMutableObjectField: string => bool;

let mangleObjectField: string => string;

let moduleItemGen: unit => moduleItemGen;

let newModuleItem: (~name: string, moduleItemGen) => moduleItem;

let newRecordValue: (~unboxed: bool, recordGen) => recordValue;

let recordGen: unit => recordGen;

let recordValueToString: recordValue => string;

let jsVariantTag: (~config: config, ~polymorphic: bool) => string;

let jsVariantValue: (~config: config, ~polymorphic: bool) => string;
