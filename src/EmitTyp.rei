open GenTypeCommon;

let blockTagValue: (~language: language, int) => string;

let commentBeforeRequire: (~language: language) => string;

let componentExportName:
  (~language: language, ~moduleName: ModuleName.t) => string;

let emitExportVariantType:
  (
    ~language: language,
    ~emitters: Emitter.emitters,
    ~name: string,
    ~typeParams: list(typ),
    ~leafTypes: list(typ)
  ) =>
  Emitter.emitters;

let emitExportType:
  (
    ~language: language,
    ~opaque: bool,
    ~typeName: string,
    ~typeVars: list(string),
    ~comment: option(string),
    typ
  ) =>
  string;

let emitExportConst:
  (~name: string, ~typ: typ, ~config: config, string) => string;

let emitExportConstMany:
  (~name: string, ~typ: typ, ~config: config, list(string)) => string;

let emitExportFunction: (~name: string, ~config: config, string) => string;

let emitExportDefault: (~config: config, string) => string;

let fileHeader: (~language: language) => string;

let generatedModuleExtension: (~language: language) => string;

let genericsString: (~typeVars: list(string)) => string;

let requireReact:
  (~language: language, ~emitters: Emitter.emitters) => Emitter.emitters;

let emitImportTypeAs:
  (
    ~language: language,
    ~emitters: Emitter.emitters,
    ~typeName: string,
    ~asTypeName: option(string),
    ~importPath: ImportPath.t
  ) =>
  Emitter.emitters;

let emitRequire: (~language: language, ModuleName.t, ImportPath.t) => string;

let ofType: (~language: language, ~typ: typ, string) => string;

let outputFileSuffix: (~language: language) => string;

let reactComponentType: (~language: language, ~propsTypeName: string) => typ;

let shimExtension: (~language: language) => string;

let typToString: (~language: language, typ) => string;