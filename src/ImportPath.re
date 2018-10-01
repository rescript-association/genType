type t = string;

let react = "react";
let reasonReact = "reason-react/src/ReasonReact.js";
let bsPlatformBlock = "bs-platform/lib/js/block.js";

let fromModule = (~dir, ~importExtension, moduleName) =>
  Filename.concat(
    dir,
    (moduleName |> ModuleName.toString) ++ importExtension,
  );

let fromStringUnsafe = s => s;

let toCmt = (~outputFileRelative, s) =>
  Filename.(concat(outputFileRelative |> dirname, s ++ ".cmt"));
let toString = s => s;