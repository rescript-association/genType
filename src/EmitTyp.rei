open GenTypeCommon;

let componentExportName:
  (~config: config, ~fileName: ModuleName.t, ~moduleName: ModuleName.t) =>
  string;

let emitExportConst:
  (
    ~comment: string=?,
    ~emitters: Emitters.t,
    ~name: string,
    ~typeNameIsInterface: string => bool,
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
    ~typeNameIsInterface: string => bool,
    ~typ: typ,
    ~config: config,
    string
  ) =>
  Emitters.t;

let emitExportConstMany:
  (
    ~emitters: Emitters.t,
    ~name: string,
    ~typeNameIsInterface: string => bool,
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
    ~early: bool=?,
    ~emitters: Emitters.t,
    ~config: config,
    ~opaque: bool,
    ~typeVars: list(string),
    ~optTyp: option(typ),
    ~typeNameIsInterface: string => bool,
    string
  ) =>
  Emitters.t;

let emitExportVariantType:
  (
    ~emitters: Emitters.t,
    ~config: config,
    ~name: string,
    ~typeParams: list(typ),
    ~typeNameIsInterface: string => bool,
    ~variants: list(variant)
  ) =>
  Emitters.t;

let emitImportTypeAs:
  (
    ~emitters: Emitters.t,
    ~config: config,
    ~typeName: string,
    ~asTypeName: option(string),
    ~typeNameIsInterface: string => bool,
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
    ~config: config,
    ~moduleName: ModuleName.t,
    ~strict: bool,
    ImportPath.t
  ) =>
  Emitters.t;

let emitRequireReact:
  (~early: bool, ~emitters: Emitters.t, ~config: config) => Emitters.t;

let emitTypeCast:
  (~config: config, ~typ: typ, ~typeNameIsInterface: string => bool, string) =>
  string;

let fileHeader: (~config: config) => string;

let generatedModuleExtension: (~config: config) => string;

let ofType:
  (~config: config, ~typeNameIsInterface: string => bool, ~typ: typ, string) =>
  string;

let ofTypeAny: (~config: config, string) => string;

let outputFileSuffix: (~config: config) => string;

let reactComponentType: (~config: config, ~propsTypeName: string) => typ;

let shimExtension: (~config: config) => string;

let typToString:
  (~config: config, ~typeNameIsInterface: string => bool, typ) => string;