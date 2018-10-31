module ModuleNameMap = Map.Make(ModuleName);

let projectRoot = ref("");

type language =
  | Flow
  | Typescript
  | Untyped;

type module_ =
  | CommonJS
  | ES6;

type importPath =
  | Relative
  | Node;

type config = {
  bsBlockPath: string,
  bsCurryPath: string,
  emitInterfaces: bool,
  importPath,
  language,
  module_,
  modulesMap: ModuleNameMap.t(ModuleName.t),
  reasonReactPath: string,
};

let default = {
  bsBlockPath: "bs-platform/lib/js/block.js",
  bsCurryPath: "bs-platform/lib/js/curry.js",
  emitInterfaces:false,
  importPath: Relative,
  language: Flow,
  module_: ES6,
  modulesMap: ModuleNameMap.empty,
  reasonReactPath: "reason-react/src/ReasonReact.js",
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
      map
      |> String_map.iter(Debug.setItem)
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

let readConfig = (~getConfigFile, ~getBsConfigFile) => {
  let fromJson = json => {
    let languageString = json |> getString("language");
    let moduleString = json |> getString("module");
    let importPathString = json |> getString("importPath");
    let reasonReactPathString = json |> getString("reasonReactPath");
    let bsBlockPathString = json |> getString("bsBlockPath");
    let bsCurryPathString = json |> getString("bsCurryPath");
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
      | "typescript" => Typescript
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
      | "" => default.bsBlockPath
      | _ => bsBlockPathString
      };
    let bsCurryPath =
      switch (bsCurryPathString) {
      | "" => default.bsCurryPath
      | _ => bsCurryPathString
      };
    {
      ...default,
      language,
      module_,
      importPath,
      reasonReactPath,
      bsBlockPath,
      bsCurryPath,
      modulesMap,
    };
  };

  let fromBsConfig = json =>
    switch (json) {
    | Ext_json_types.Obj({map, _}) =>
      switch (map |> String_map.find_opt("gentypeconfig")) {
      | Some(jsonGenFlowConfig) => jsonGenFlowConfig |> fromJson
      | _ => default
      }
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