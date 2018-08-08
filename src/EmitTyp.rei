open GenFlowCommon;

let blockTagValue: (~language: language, int) => string;

let commentBeforeRequire: (~language: language) => string;

let componentExportName:
  (~language: language, ~moduleName: ModuleName.t) => string;

let emitExportVariantType:
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
    ~comment: option(string),
    typ
  ) =>
  string;

let fileHeader: (~language: language) => string;

let generatedModuleExtension: (~language: language) => string;

let genericsString: list(string) => string;

let importReact: (~language: language) => string;

let emitImportTypeAs:
  (
    ~language: language,
    ~typeName: string,
    ~asTypeName: option(string),
    ~importPath: ImportPath.t
  ) =>
  string;

let ofType: (~language: language, ~typ: typ, string) => string;

let outputFileSuffix: (~language: language) => string;

let reactComponentType: (~language: language) => string;

let requireReact: (~language: language) => bool;

let shimExtension: (~language: language) => string;

let typToString: (~language: language, typ) => string;