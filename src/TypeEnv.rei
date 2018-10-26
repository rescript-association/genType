type t;

let addModulePath: (~typeEnv: t, string) => string;

let addModuleTypeSignature:
  (~name: string, ~signature: Typedtree.signature, t) => unit;

let getCurrentModuleName: (~fileName: ModuleName.t, t) => ModuleName.t;

let getValueAccessPath: (~name: string, t) => string;

let lookup: (~name: string, t) => option(t);

let lookupModuleTypeSignature:
  (~path: Path.t, t) => option(Typedtree.signature);

let newModule: (~name: string, t) => t;

let newType: (~name: string, t) => unit;

let root: unit => t;

let toString: t => string;

let updateModuleItem: (~moduleItem: Runtime.moduleItem, t) => unit;