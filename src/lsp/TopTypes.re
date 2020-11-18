/* Aliases to make the intents clearer */
type uri = string;
type filePath = string;
type moduleName = string;

/* Here are the things that will be different between jbuilder things */
type package = {
  rootPath: filePath,
  /* Might change based on bsconfig.json / .merlin */
  includeDirectories: list(filePath),
  compilationFlags: string,
  /* Depend on bsb having already run */
  localModules: list(moduleName),
  interModuleDependencies: Hashtbl.t(moduleName, list(moduleName)),
  dependencyModules: list(moduleName),
  pathsForModule: Hashtbl.t(moduleName, SharedTypes.paths),
  nameForPath: Hashtbl.t(filePath, moduleName),
  namespace: option(string),
  opens: list(string),
  tmpPath: string,
  mutable rebuildTimer: float,
  buildCommand: option(string),
  compilerPath: filePath,
  mlfmtPath: option(filePath),
  refmtPath: option(filePath),
  /** TODO maybe make this general, so that I can support arbitrary syntaxes? */
  lispRefmtPath: option(filePath),
};

type settings = {
  formatWidth: option(int),
  perValueCodelens: bool,
  mlfmtLocation: option(string),
  refmtLocation: option(string),
  lispRefmtLocation: option(string),
  opensCodelens: bool,
  dependenciesCodelens: bool,
  clientNeedsPlainText: bool,
  showModulePathOnHover: bool,
  recordAllLocations: bool,
  autoRebuild: bool,
};

type state = {
  rootPath: filePath,
  rootUri: uri,
  settings,
  documentText: Hashtbl.t(uri, (string, int, bool)),
  documentTimers: Hashtbl.t(uri, float),
  packagesByRoot: Hashtbl.t(string, package),
  rootForUri: Hashtbl.t(uri, string),
  cmtCache: Hashtbl.t(filePath, (float, SharedTypes.file)),
  compiledDocuments: Hashtbl.t(uri, AsYouType.result),
  lastDefinitions: Hashtbl.t(uri, SharedTypes.full),
};

let empty = () => {
  rootPath: "- uninitialized -",
  rootUri: "- uninitialized -",
  documentText: Hashtbl.create(5),
  documentTimers: Hashtbl.create(10),
  packagesByRoot: Hashtbl.create(1),
  rootForUri: Hashtbl.create(30),
  cmtCache: Hashtbl.create(30),
  compiledDocuments: Hashtbl.create(10),
  lastDefinitions: Hashtbl.create(10),
  settings: {
    formatWidth: None,
    mlfmtLocation: None,
    refmtLocation: None,
    lispRefmtLocation: None,
    perValueCodelens: false,
    opensCodelens: true,
    dependenciesCodelens: true,
    clientNeedsPlainText: false,
    showModulePathOnHover: true,
    recordAllLocations: false,
    autoRebuild: true,
  },
};