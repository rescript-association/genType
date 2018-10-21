open GenTypeCommon;

let blockTagValue: (~language: language, int) => string;

let componentExportName:
  (~language: language, ~fileName: ModuleName.t, ~moduleName: ModuleName.t) =>
  string;

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
  (
    ~early: bool,
    ~comment: string=?,
    ~emitters: Emitters.t,
    ~name: string,
    ~config: config,
    string
  ) =>
  Emitters.t;

let emitExportType:
  (
    ~early: bool,
    ~emitters: Emitters.t,
    ~language: language,
    ~opaque: bool,
    ~optTyp: option(typ),
    ~typeVars: list(string),
    string
  ) =>
  Emitters.t;

let emitExportVariantType:
  (
    ~emitters: Emitters.t,
    ~language: language,
    ~name: string,
    ~typeParams: list(typ),
    ~variants: list(variant)
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
  (
    ~emitters: Emitters.t,
    ~name: string,
    ~nameAs: option(string),
    ImportPath.t
  ) =>
  Emitters.t;

let emitRequire:
  (
    ~early: bool,
    ~emitters: Emitters.t,
    ~language: language,
    ~moduleName: ModuleName.t,
    ~strict: bool,
    ImportPath.t
  ) =>
  Emitters.t;

let emitRequireReact:
  (~early: bool, ~emitters: Emitters.t, ~language: language) => Emitters.t;

let fileHeader: (~language: language) => string;

let generatedModuleExtension: (~language: language) => string;

let ofType: (~language: language, ~typ: typ, string) => string;

let ofTypeAny: (~language: language, string) => string;

let outputFileSuffix: (~language: language) => string;

let reactComponentType: (~language: language, ~propsTypeName: string) => typ;

let shimExtension: (~language: language) => string;

let typToString: (~language: language, typ) => string;