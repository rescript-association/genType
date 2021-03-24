module ModuleNameMap = Map.Make(ModuleName);

let bsbProjectRoot = ref("");
let projectRoot = ref("");

type language =
  | Flow
  | TypeScript
  | Untyped;

type module_ =
  | CommonJS
  | ES6;

type importPath =
  | Relative
  | Node;

type bsVersion = (int, int, int);

type config = {
  bsCurryPath: option(string),
  bsDependencies: list(string),
  mutable emitFlowAny: bool,
  mutable emitImportCurry: bool,
  mutable emitImportPropTypes: bool,
  mutable emitImportReact: bool,
  mutable emitTypePropDone: bool,
  exportInterfaces: bool,
  fileHeader: option(string),
  generatedFileExtension: option(string),
  importPath,
  language,
  module_,
  namespace: option(string),
  platformLib: string,
  propTypes: bool,
  reasonReactPath: string,
  shimsMap: ModuleNameMap.t(ModuleName.t),
  sources: option(Ext_json_types.t),
};

let default = {
  bsCurryPath: None,
  bsDependencies: [],
  emitFlowAny: false,
  emitImportCurry: false,
  emitImportPropTypes: false,
  emitImportReact: false,
  emitTypePropDone: false,
  exportInterfaces: false,
  fileHeader: None,
  generatedFileExtension: None,
  importPath: Relative,
  language: Flow,
  module_: ES6,
  namespace: None,
  platformLib: "",
  propTypes: false,
  reasonReactPath: "reason-react/src/ReasonReact.js",
  shimsMap: ModuleNameMap.empty,
  sources: None,
};

let bsPlatformLib = (~config) =>
  switch (config.module_) {
  | ES6 => config.platformLib ++ "/lib/es6"
  | CommonJS => config.platformLib ++ "/lib/js"
  };

let bsPlatformLibExtension = ".js";

let getBsCurryPath = (~config) =>
  switch (config.bsCurryPath) {
  | None => bsPlatformLib(~config) ++ "/curry" ++ bsPlatformLibExtension
  | Some(s) => s
  };

type map = String_map.t(Ext_json_types.t);

let getOpt = (s, map: map) => String_map.find_opt(s, map);

let getBool = (s, map) =>
  switch (map |> getOpt(s)) {
  | Some(True(_)) => Some(true)
  | Some(False(_)) => Some(false)
  | _ => None
  };

let getString = (s, map) =>
  switch (map |> getOpt(s)) {
  | Some(Str({str})) => str
  | _ => ""
  };

let getStringOption = (s, map) =>
  switch (map |> getOpt(s)) {
  | Some(Str({str})) => Some(str)
  | _ => None
  };

let getShims = map => {
  let shims = ref([]);
  switch (map |> getOpt("shims")) {
  | Some(Obj({map: shimsMap})) =>
    shimsMap
    |> String_map.iter((fromModule, toModule) =>
         switch (toModule) {
         | Ext_json_types.Str({str}) =>
           shims := [(fromModule, str), ...shims^]
         | _ => ()
         }
       )
  | Some(Arr({content})) =>
    /* To be deprecated: array of strings */
    content
    |> Array.iter(x =>
         switch (x) {
         | Ext_json_types.Str({str}) =>
           let fromTo = Str.split(Str.regexp("="), str) |> Array.of_list;
           assert(Array.length(fromTo) === 2);
           shims := [(fromTo[0], fromTo[1]), ...shims^];
         | _ => ()
         }
       )
  | _ => ()
  };
  shims^;
};

let setDebug = (~gtconf) =>
  switch (gtconf |> getOpt("debug")) {
  | Some(Obj({map})) => map |> String_map.iter(Debug.setItem)
  | _ => ()
  };

let readConfig = (~bsVersion, ~getBsConfigFile, ~namespace) => {
  let parseConfig = (~bsconf, ~gtconf) => {
    let languageString = gtconf |> getString("language");
    let moduleString = gtconf |> getStringOption("module");
    let importPathString = gtconf |> getString("importPath");
    let reasonReactPathString = gtconf |> getString("reasonReactPath");
    let fileHeader = gtconf |> getStringOption("fileHeader");
    let bsCurryPathString = gtconf |> getString("bsCurryPath");
    let exportInterfacesBool = gtconf |> getBool("exportInterfaces");
    let generatedFileExtensionStringOption =
      gtconf |> getStringOption("generatedFileExtension");
    let propTypesBool = gtconf |> getBool("propTypes");
    let shimsMap =
      gtconf
      |> getShims
      |> List.fold_left(
           (map, (fromModule, toModule)) => {
             let moduleName: ModuleName.t =
               fromModule |> ModuleName.fromStringUnsafe;
             let shimModuleName = toModule |> ModuleName.fromStringUnsafe;
             ModuleNameMap.add(moduleName, shimModuleName, map);
           },
           ModuleNameMap.empty,
         );
    setDebug(~gtconf);
    let language =
      switch (languageString) {
      | "typescript" => TypeScript
      | "untyped" => Untyped
      | _ => Flow
      };
    let module_ = {
      let packageSpecsModuleString =
        switch (bsconf |> getOpt("package-specs")) {
        | Some(Obj({map: packageSpecs})) =>
          packageSpecs |> getStringOption("module")
        | _ => None
        };
      // Give priority to gentypeconfig, followed by package-specs
      switch (moduleString, packageSpecsModuleString) {
      | (Some("commonjs"), _) => CommonJS
      | (Some("es6"), _) => ES6
      | (None, Some("commonjs")) => CommonJS
      | (None, Some("es6" | "es6-global")) => ES6
      | _ => default.module_
      };
    };
    let importPath =
      switch (importPathString) {
      | "relative" => Relative
      | "node" => Node
      | _ => default.importPath
      };
    let reasonReactPath =
      switch (reasonReactPathString) {
      | "" => default.reasonReactPath
      | _ => reasonReactPathString
      };
    let bsCurryPath =
      switch (bsCurryPathString) {
      | "" => None
      | _ => Some(bsCurryPathString)
      };
    let exportInterfaces =
      switch (exportInterfacesBool) {
      | None => default.exportInterfaces
      | Some(b) => b
      };
    let propTypes =
      switch (propTypesBool) {
      | None => default.propTypes
      | Some(b) => b
      };
    let fileHeader = fileHeader;
    let generatedFileExtension = generatedFileExtensionStringOption;
    let bsVersion =
      switch (bsVersion) {
      | None => (0, 0, 0)
      | Some(s) =>
        switch (s |> Str.split(Str.regexp(Str.quote(".")))) {
        | [x1, x2, x3, ..._] =>
          let v1 = int_of_string(x1);
          let v2 = int_of_string(x2);
          let v3 =
            switch (x3 |> Str.split(Str.regexp("-"))) {
            | [x3, ..._] => int_of_string(x3)
            | _ => 0
            };
          (v1, v2, v3);
        | _ => (0, 0, 0)
        }
      };
    let (v1, v2, v3) = bsVersion;
    let platformLib =
      if (v1 >= 9 && v2 >= 1) {
        "rescript";
      } else {
        "bs-platform";
      };
    if (Debug.config^) {
      Log_.item("Project root: %s\n", projectRoot^);
      if (bsbProjectRoot^ != projectRoot^) {
        Log_.item("bsb project root: %s\n", bsbProjectRoot^);
      };
      Log_.item(
        "Config language:%s module:%s importPath:%s shims:%d entries bsVersion:%d.%d.%d\n",
        languageString,
        switch (moduleString) {
        | None => ""
        | Some(s) => s
        },
        importPathString,
        shimsMap |> ModuleNameMap.cardinal,
        v1,
        v2,
        v3,
      );
    };

    let namespace =
      switch (bsconf |> getOpt("namespace")) {
      | Some(True(_)) => namespace
      | _ => default.namespace
      };

    let bsDependencies =
      switch (bsconf |> getOpt("bs-dependencies")) {
      | Some(Arr({content})) =>
        let strings = ref([]);
        content
        |> Array.iter(x =>
             switch (x) {
             | Ext_json_types.Str({str}) => strings := [str, ...strings^]
             | _ => ()
             }
           );
        strings^;
      | _ => default.bsDependencies
      };

    let sources =
      switch (bsconf |> getOpt("sources")) {
      | Some(sourceItem) => Some(sourceItem)
      | _ => default.sources
      };

    {
      bsCurryPath,
      bsDependencies,
      emitFlowAny: false,
      emitImportCurry: false,
      emitImportPropTypes: false,
      emitImportReact: false,
      emitTypePropDone: false,
      exportInterfaces,
      fileHeader,
      generatedFileExtension,
      importPath,
      language,
      module_,
      namespace,
      platformLib,
      propTypes,
      reasonReactPath,
      shimsMap,
      sources,
    };
  };
  switch (getBsConfigFile()) {
  | Some(bsConfigFile) =>
    try({
      let json = bsConfigFile |> Ext_json_parse.parse_json_from_file;
      switch (json) {
      | Obj({map: bsconf}) =>
        switch (bsconf |> getOpt("gentypeconfig")) {
        | Some(Obj({map: gtconf})) => parseConfig(~bsconf, ~gtconf)
        | _ => default
        }
      | _ => default
      };
    }) {
    | _ => default
    }
  | None => default
  };
};
