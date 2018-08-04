type t;
let bsPlatformBlock: t;
let fromModule: (~dir: string, ~ext: string, ModuleName.t) => t;
let fromStringUnsafe : string => t;
let reasonReact: t;
let react: t;
let toString: t => string;