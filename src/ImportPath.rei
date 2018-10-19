open GenTypeCommon;

type t;

let bsBlockPath: (~config: config) => t;

let bsCurryPath: (~config: config) => t;

let fromModule:
  (~config: config, ~dir: string, ~importExtension: string, ModuleName.t) => t;

let fromStringUnsafe: string => t;

let reasonReactPath: (~config: config) => t;

let react: t;

let toCmt: (~outputFileRelative: string, t) => string;

let toString: t => string;