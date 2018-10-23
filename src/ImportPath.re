open GenTypeCommon;

type t = string;

let react = "react";
let reasonReactPath = (~config) => config.reasonReactPath;
let bsBlockPath = (~config) => config.bsBlockPath;

let bsCurryPath = (~config) => config.bsCurryPath;

module Filename2 = {
  include Filename;

  /* Force "/" separator on all platforms. */
  let dir_sep = "/";

  let concat = (dirname, filename) => {
    let is_dir_sep = (s, i) => {
      let c = s.[i];
      c == '/' || c == '\\' || c == ':';
    };
    let l = String.length(dirname);
    if (l == 0 || is_dir_sep(dirname, l - 1)) {
      dirname ++ filename;
    } else {
      dirname ++ dir_sep ++ filename;
    };
  };
};

let fromModule = (~config, ~dir, ~importExtension, moduleName) => {
  let withNoPath = (moduleName |> ModuleName.toString) ++ importExtension;
  switch (config.importPath) {
  | Relative => Filename2.concat(dir, withNoPath)
  | Node => withNoPath
  };
};

let fromStringUnsafe = s => s;

let toCmt = (~outputFileRelative, s) =>
  Filename.(concat(outputFileRelative |> dirname, s ++ ".cmt"));
let toString = s => s;