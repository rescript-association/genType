type t;
let bsPlatformBlock: t;
let fromModule: (~dir: string, ~importExtension: string, ModuleName.t) => t;
let fromStringUnsafe: string => t;
let reasonReact: t;
let react: t;

let toCmt: (~outputFileRelative: string, t) => string;
let toString: t => string;