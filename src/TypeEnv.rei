type t;

let addModulePath: (~typeEnv: t, string) => string;

let addModuleTypeSignature:
  (~name: string, ~signature: Typedtree.signature, t) => unit;

let getCurrentModuleName: (~fileName: ModuleName.t, t) => ModuleName.t;

/* Access path for the value in the module.
   It can be the value name if the module is not nested.
   Or TopLevelModule[x][y] if accessing a value in a doubly nested module */
let getValueAccessPath: (~component: bool=?, ~name: string, t) => string;

let lookup: (~name: string, t) => option(t);

let lookupModuleTypeSignature:
  (~path: Path.t, t) => option(Typedtree.signature);

let newModule: (~name: string, t) => t;

let newType: (~name: string, t) => unit;

let root: unit => t;

let toString: t => string;

let updateModuleItem:
  (~nameOpt: option(string)=?, ~moduleItem: Runtime.moduleItem, t) => unit;