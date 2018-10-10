open GenTypeCommon;

let blockTagValue: (~language: language, int) => string;

let componentExportName:
  (~language: language, ~moduleName: ModuleName.t) => string;

let emitExportConst:
  (~name: string, ~typ: typ, ~config: config, string) => string;

let emitExportConstMany:
  (~name: string, ~typ: typ, ~config: config, list(string)) => string;

let emitExportDefault: (~config: config, string) => string;

let emitExportFunction: (~name: string, ~config: config, string) => string;

let emitExportType:
  (
    ~language: language,
    ~emitters: Emitter.emitters,
    ~opaque: bool,
    ~typeName: string,
    ~typeVars: list(string),
    ~comment: option(string),
    typ
  ) =>
  Emitter.emitters;

let emitExportVariantType:
  (
    ~language: language,
    ~emitters: Emitter.emitters,
    ~name: string,
    ~typeParams: list(typ),
    ~leafTypes: list(typ)
  ) =>
  Emitter.emitters;

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

let fileHeader: (~language: language) => string;

let generatedModuleExtension: (~language: language) => string;

let ofType: (~language: language, ~typ: typ, string) => string;

let outputFileSuffix: (~language: language) => string;

let reactComponentType: (~language: language, ~propsTypeName: string) => typ;

let requireReact:
  (~language: language, ~emitters: Emitter.emitters) => Emitter.emitters;

let shimExtension: (~language: language) => string;

let typToString: (~language: language, typ) => string;