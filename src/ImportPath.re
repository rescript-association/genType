open GenTypeCommon;

type t = string;

let propTypes = "prop-types";

let react = "react";
let reasonReactPath = (~config) => config.reasonReactPath;
let bsBlockPath = Config_.getBsBlockPath;

let bsCurryPath = Config_.getBsCurryPath;

let fromModule = (~config, ~dir, ~importExtension, moduleName) => {
  let withNoPath = (moduleName |> ModuleName.toString) ++ importExtension;
  switch (config.importPath) {
  | Relative => NodeFilename.concat(dir, withNoPath)
  | Node => withNoPath
  };
};

let fromStringUnsafe = s => s;

let chopExtensionSafe = s =>
  try (s |> Filename.chop_extension) {
  | Invalid_argument(_) => s
  };

let toCmt = (~config, ~outputFileRelative, s) =>
  Filename.(
    concat(
      outputFileRelative |> dirname,
      (s |> chopExtensionSafe)
      ++ (
        switch (config.namespace) {
        | None => ""
        | Some(name) => "-" ++ name
        }
      )
      ++ ".cmt",
    )
  );
let toString = s => s;