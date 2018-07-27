type t = string;

let react = "React";
let bsPlatformBlock = "bs-platform/lib/js/block.js";

let reasonReact = "reason-react/src/ReasonReact.js";
let fromModule = (~dir, ~ext, moduleName) =>
  Filename.concat(dir, (moduleName |> ModuleName.toString) ++ ext);
let toString = s => s;