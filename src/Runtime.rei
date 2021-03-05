open GenTypeCommon;

type recordGen;

type recordValue;

type moduleItem;

type moduleAccessPath =
  | Root(string)
  | Dot(moduleAccessPath, moduleItem);

let accessVariant: (~index: int, string) => string;

let checkMutableObjectField: (~previousName: string, ~name: string) => bool;

/* Internal name of a value called "default" used by buclescript for default export */
let default: string;

let emitModuleAccessPath: (~config: config, moduleAccessPath) => string;

let emitJSVariantGetLabel: (~polymorphic: bool, string) => string;

let emitJSVariantGetPayload: (~polymorphic: bool, string) => string;

let emitJSVariantWithPayload:
  (~label: string, ~polymorphic: bool, string) => string;

let emitVariantGetLabel: (~polymorphic: bool, string) => string;

let emitVariantGetPayload:
  (~inlineRecord: bool, ~numArgs: int, ~polymorphic: bool, string) => string;

let emitVariantLabel: (~polymorphic: bool, string) => string;

let emitVariantWithPayload:
  (
    ~config: Config_.config,
    ~inlineRecord: bool,
    ~label: string,
    ~polymorphic: bool,
    list(string)
  ) =>
  string;

let isMutableObjectField: string => bool;

let mangleObjectField: string => string;

let newModuleItem: (~name: string) => moduleItem;

let newRecordValue: (~unboxed: bool, recordGen) => recordValue;

let recordGen: unit => recordGen;

let recordValueToString: recordValue => string;

let jsVariantTag: (~polymorphic: bool) => string;

let jsVariantValue: (~polymorphic: bool) => string;
