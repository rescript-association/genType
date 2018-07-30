type t;
let bsPlatformBlock: t;
let fromModule: (~dir: string, ~ext: string, ModuleName.t) => t;
let fromStringUnsafe : string => t;
let react: t;
let reasonReact: t;
let reactjs: t;
let toString: t => string;