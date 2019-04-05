module ModuleNameMap = Map.Make(ModuleName);

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

type config = {
  bsBlockPath: option(string),
  bsCurryPath: option(string),
  mutable emitCreateBucklescriptBlock: bool,
  mutable emitFlowAny: bool,
  mutable emitImportCurry: bool,
  mutable emitImportPropTypes: bool,
  exportInterfaces: bool,
  generatedFileExtension: option(string),
  importPath,
  language,
  module_,
  modulesMap: ModuleNameMap.t(ModuleName.t),
  namespace: option(string),
  reasonReactPath: string,
};

let default = {
  bsBlockPath: None,
  bsCurryPath: None,
  emitCreateBucklescriptBlock: false,
  emitFlowAny: false,
  emitImportCurry: false,
  emitImportPropTypes: false,
  exportInterfaces: false,
  generatedFileExtension: None,
  importPath: Relative,
  language: Flow,
  module_: ES6,
  modulesMap: ModuleNameMap.empty,
  namespace: None,
  reasonReactPath: "reason-react/src/ReasonReact.js",
};

let bsPlatformLib = (~config) =>
  switch (config.module_) {
  | ES6 => "bs-platform/lib/es6"
  | CommonJS => "bs-platform/lib/js"
  };

let getBsBlockPath = (~config) =>
  switch (config.bsBlockPath) {
  | None => bsPlatformLib(~config) ++ "/block.js"
  | Some(s) => s
  };

let getBsCurryPath = (~config) =>
  switch (config.bsCurryPath) {
  | None => bsPlatformLib(~config) ++ "/curry.js"
  | Some(s) => s
  };

let getBool = (s, json) =>
  switch (json) {
  | Ext_json_types.Obj({map, _}) =>
    switch (map |> String_map.find_opt(s)) {
    | Some(True(_)) => Some(true)
    | Some(False(_)) => Some(false)
    | _ => None
    }
  | _ => None
  };

let getString = (s, json) =>
  switch (json) {
  | Ext_json_types.Obj({map, _}) =>
    switch (map |> String_map.find_opt(s)) {
    | Some(Str({str, _})) => str
    | _ => ""
    }
  | _ => ""
  };

let getStringOption = (s, json) =>
  switch (json) {
  | Ext_json_types.Obj({map, _}) =>
    switch (map |> String_map.find_opt(s)) {
    | Some(Str({str, _})) => Some(str)
    | _ => None
    }
  | _ => None
  };

let getShims = json => {
  let shims = ref([]);
  switch (json) {
  | Ext_json_types.Obj({map, _}) =>
    switch (map |> String_map.find_opt("shims")) {
    | Some(Ext_json_types.Obj({map: shimsMap, _})) =>
      shimsMap
      |> String_map.iter((fromModule, toModule) =>
           switch (toModule) {
           | Ext_json_types.Str({str, _}) =>
             shims := [(fromModule, str), ...shims^]
           | _ => ()
           }
         )
    | Some(Arr({content, _})) =>
      /* To be deprecated: array of strings */
      content
      |> Array.iter(x =>
           switch (x) {
           | Ext_json_types.Str({str, _}) =>
             let fromTo = Str.split(Str.regexp("="), str) |> Array.of_list;
             assert(Array.length(fromTo) === 2);
             shims := [(fromTo[0], fromTo[1]), ...shims^];
           | _ => ()
           }
         );
      ();
    | _ => ()
    }
  | _ => ()
  };
  shims^;
};

let getDebug = json =>
  switch (json) {
  | Ext_json_types.Obj({map, _}) =>
    switch (map |> String_map.find_opt("debug")) {
    | Some(Ext_json_types.Obj({map, _})) =>
      map |> String_map.iter(Debug.setItem)
    | _ => ()
    }
  | _ => ()
  };

let logFile = ref(None);

let getLogFile = () =>
  switch (logFile^) {
  | None =>
    let f =
      open_out_gen(
        [Open_creat, Open_text, Open_append],
        0o640,
        Filename.concat(projectRoot^, ".genTypeLog"),
      );
    logFile := Some(f);
    f;
  | Some(f) => f
  };

let logItem = x => {
  let outChannel =
    switch (Debug.channel) {
    | Stdout => stdout
    | Logfile => getLogFile()
    };
  Printf.fprintf(outChannel, "  ");
  Printf.fprintf(outChannel, x);
};

let readConfig = (~getConfigFile, ~getBsConfigFile, ~namespace) => {
  let fromJson = json => {
    let languageString = json |> getString("language");
    let moduleString = json |> getString("module");
    let importPathString = json |> getString("importPath");
    let reasonReactPathString = json |> getString("reasonReactPath");
    let bsBlockPathString = json |> getString("bsBlockPath");
    let bsCurryPathString = json |> getString("bsCurryPath");
    let exportInterfacesBool = json |> getBool("exportInterfaces");
    let generatedFileExtensionStringOption =
      json |> getStringOption("generatedFileExtension");
    let modulesMap =
      json
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
    json |> getDebug;
    if (Debug.config^) {
      logItem(
        "Config language:%s module:%s importPath:%s shims:%d entries\n",
        languageString,
        moduleString,
        importPathString,
        modulesMap |> ModuleNameMap.cardinal,
      );
    };
    let language =
      switch (languageString) {
      | "typescript" => TypeScript
      | "untyped" => Untyped
      | _ => Flow
      };
    let module_ =
      switch (moduleString) {
      | "commonjs" => CommonJS
      | "es6" => ES6
      | _ => default.module_
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
    let bsBlockPath =
      switch (bsBlockPathString) {
      | "" => None
      | _ => Some(bsBlockPathString)
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
    let generatedFileExtension = generatedFileExtensionStringOption;

    {
      bsBlockPath,
      bsCurryPath,
      emitCreateBucklescriptBlock: false,
      emitFlowAny: false,
      emitImportCurry: false,
      emitImportPropTypes: false,
      exportInterfaces,
      generatedFileExtension,
      importPath,
      language,
      module_,
      modulesMap,
      namespace: None,
      reasonReactPath,
    };
  };

  let fromBsConfig = json =>
    switch (json) {
    | Ext_json_types.Obj({map, _}) =>
      let config =
        switch (map |> String_map.find_opt("gentypeconfig")) {
        | Some(jsonGenFlowConfig) => jsonGenFlowConfig |> fromJson
        | _ => default
        };
      switch (map |> String_map.find_opt("namespace")) {
      | Some(True(_)) => {...config, namespace}
      | _ => config
      };
    | _ => default
    };
  switch (getConfigFile()) {
  | Some(configFile) =>
    try (configFile |> Ext_json_parse.parse_json_from_file |> fromJson) {
    | _ => default
    }
  | None =>
    switch (getBsConfigFile()) {
    | Some(bsConfigFile) =>
      try (bsConfigFile |> Ext_json_parse.parse_json_from_file |> fromBsConfig) {
      | _ => default
      }
    | None => default
    }
  };
};