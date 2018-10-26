module ModuleNameMap = Map.Make(ModuleName);

type language =
  | Flow
  | Typescript
  | Untyped;

type module_ =
  | CommonJS
  | ES6;

type importPath =
  | Relative
  | Node;

type config = {
  bsBlockPath: string,
  bsCurryPath: string,
  importPath,
  inlineAnnotations: bool,
  language,
  module_,
  modulesMap: ModuleNameMap.t(ModuleName.t),
  reasonReactPath: string,
};

let default = {
  bsBlockPath: "bs-platform/lib/js/block.js",
  bsCurryPath: "bs-platform/lib/js/curry.js",
  importPath: Relative,
  inlineAnnotations: true,
  language: Flow,
  module_: ES6,
  modulesMap: ModuleNameMap.empty,
  reasonReactPath: "reason-react/src/ReasonReact.js",
};