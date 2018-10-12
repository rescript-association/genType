open GenTypeCommon;

let blockTagValue: (~language: language, int) => string;

let componentExportName:
  (~language: language, ~moduleName: ModuleName.t) => string;

let emitExportConst:
  (
    ~comment: string=?,
    ~emitters: Emitters.t,
    ~name: string,
    ~typ: typ,
    ~config: config,
    string
  ) =>
  Emitters.t;

let emitExportConstEarly:
  (
    ~comment: string=?,
    ~emitters: Emitters.t,
    ~name: string,
    ~typ: typ,
    ~config: config,
    string
  ) =>
  Emitters.t;

let emitExportConstMany:
  (
    ~emitters: Emitters.t,
    ~name: string,
    ~typ: typ,
    ~config: config,
    list(string)
  ) =>
  Emitters.t;

let emitExportDefault:
  (~emitters: Emitters.t, ~config: config, string) => Emitters.t;

let emitExportFunction:
  (~emitters: Emitters.t, ~name: string, ~config: config, string) => Emitters.t;

let emitExportType:
  (
    ~emitters: Emitters.t,
    ~language: language,
    ~opaque: bool,
    ~typeName: string,
    ~typeVars: list(string),
    ~comment: option(string),
    typ
  ) =>
  Emitters.t;

let emitExportVariantType:
  (
    ~emitters: Emitters.t,
    ~language: language,
    ~name: string,
    ~typeParams: list(typ),
    ~leafTypes: list(typ)
  ) =>
  Emitters.t;

let emitImportTypeAs:
  (
    ~emitters: Emitters.t,
    ~language: language,
    ~typeName: string,
    ~asTypeName: option(string),
    ~importPath: ImportPath.t
  ) =>
  Emitters.t;

let emitImportValueAsEarly:
  (~emitters: Emitters.t, ~name: string, ~nameAs: string, ImportPath.t) =>
  Emitters.t;

let emitRequire:
  (
    ~emitters: Emitters.t,
    ~language: language,
    ~moduleName: ModuleName.t,
    ImportPath.t
  ) =>
  Emitters.t;

let emitRequireEarly:
  (
    ~emitters: Emitters.t,
    ~language: language,
    ~moduleName: ModuleName.t,
    ImportPath.t
  ) =>
  Emitters.t;

let emitRequireReact:
  (~emitters: Emitters.t, ~language: language) => Emitters.t;

let fileHeader: (~language: language) => string;

let generatedModuleExtension: (~language: language) => string;

let ofType: (~language: language, ~typ: typ, string) => string;

let outputFileSuffix: (~language: language) => string;

let reactComponentType: (~language: language, ~propsTypeName: string) => typ;

let shimExtension: (~language: language) => string;

let typToString: (~language: language, typ) => string;