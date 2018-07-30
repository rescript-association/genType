type t = string;

let reactjs = "react";
let react = "React";
let reasonReact = "reason-react/src/ReasonReact.js";
let bsPlatformBlock = "bs-platform/lib/js/block.js";

let fromModule = (~dir, ~ext, moduleName) =>
  Filename.concat(dir, (moduleName |> ModuleName.toString) ++ ext);

let fromStringUnsafe = s => s;
let toString = s => s;