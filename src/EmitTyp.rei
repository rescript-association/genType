open GenFlowCommon;

let blockTagValue: (~language: language, int) => string;

let commentBeforeRequire: (~language: language) => string;

let componentExportName:
  (~language: language, ~moduleName: ModuleName.t) => string;

let emitExportUnionType:
  (
    ~language: language,
    ~name: string,
    ~typeParams: list(typ),
    ~leafTypes: list(typ)
  ) =>
  string;

let emitExportType:
  (
    ~language: language,
    ~opaque: bool,
    ~typeName: string,
    ~typeParams: list(typ),
    string
  ) =>
  string;

let fileHeader: (~language: language) => string;

let generatedModuleExtension: (~language: language) => string;

let genericsString: list(string) => string;

let importReact: (~language: language) => string;

let importTypeAs:
  (
    ~language: language,
    ~typeName: string,
    ~asTypeName: option(string),
    ~importPath: ImportPath.t
  ) =>
  string;

let outputFileSuffix: (~language: language) => string;

let reactComponentType: (~language: language) => string;

let requireReact: (~language: language) => bool;

let shimExtension: (~language: language) => string;

let toString: (~language: language, typ) => string;