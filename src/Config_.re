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
  bsBlockPath: option(string),
  bsCurryPath: option(string),
  bsDependencies: list(string),
  bsVersion,
  mutable emitCreateBucklescriptBlock: bool,
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
  modulesAsObjects: bool,
  namespace: option(string),
  propTypes: bool,
  reasonReactPath: string,
  recordsAsObjects: bool,
  shimsMap: ModuleNameMap.t(ModuleName.t),
  sources: option(Ext_json_types.t),
  useUnboxedAnnotations: bool,
};

let default = {
  bsBlockPath: None,
  bsCurryPath: None,
  bsDependencies: [],
  bsVersion: (0, 0, 0),
  emitCreateBucklescriptBlock: false,
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
  modulesAsObjects: false,
  namespace: None,
  propTypes: false,
  reasonReactPath: "reason-react/src/ReasonReact.js",
  recordsAsObjects: false,
  shimsMap: ModuleNameMap.empty,
  sources: None,
  useUnboxedAnnotations: false,
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
  | Ext_json_types.Obj({map}) =>
    switch (map |> String_map.find_opt(s)) {
    | Some(True(_)) => Some(true)
    | Some(False(_)) => Some(false)
    | _ => None
    }
  | _ => None
  };

let getString = (s, json) =>
  switch (json) {
  | Ext_json_types.Obj({map}) =>
    switch (map |> String_map.find_opt(s)) {
    | Some(Str({str})) => str
    | _ => ""
    }
  | _ => ""
  };

let getStringOption = (s, json) =>
  switch (json) {
  | Ext_json_types.Obj({map}) =>
    switch (map |> String_map.find_opt(s)) {
    | Some(Str({str})) => Some(str)
    | _ => None
    }
  | _ => None
  };

let getShims = json => {
  let shims = ref([]);
  switch (json) {
  | Ext_json_types.Obj({map}) =>
    switch (map |> String_map.find_opt("shims")) {
    | Some(Ext_json_types.Obj({map: shimsMap})) =>
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
    }
  | _ => ()
  };
  shims^;
};

let getDebug = json =>
  switch (json) {
  | Ext_json_types.Obj({map}) =>
    switch (map |> String_map.find_opt("debug")) {
    | Some(Ext_json_types.Obj({map})) =>
      map |> String_map.iter(Debug.setItem)
    | _ => ()
    }
  | _ => ()
  };

module Color = {
  let color_enabled = lazy(Unix.isatty(Unix.stdout));

  let get_color_enabled = () => {
    Lazy.force(color_enabled);
  };

  type color =
    | Black
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White;

  type style =
    | FG(color)
    | BG(color)
    | Bold
    | Dim;

  let code_of_style =
    fun
    | FG(Black) => "30"
    | FG(Red) => "31"
    | FG(Green) => "32"
    | FG(Yellow) => "33"
    | FG(Blue) => "34"
    | FG(Magenta) => "35"
    | FG(Cyan) => "36"
    | FG(White) => "37"

    | BG(Black) => "40"
    | BG(Red) => "41"
    | BG(Green) => "42"
    | BG(Yellow) => "43"
    | BG(Blue) => "44"
    | BG(Magenta) => "45"
    | BG(Cyan) => "46"
    | BG(White) => "47"

    | Bold => "1"
    | Dim => "2";

  let style_of_tag = s =>
    switch (s) {
    | "error" => [Bold, FG(Red)]
    | "warning" => [Bold, FG(Magenta)]
    | "info" => [Bold, FG(Yellow)]
    | "dim" => [Dim]
    | "filename" => [FG(Cyan)]
    | _ => []
    };

  let ansi_of_tag = s => {
    let l = style_of_tag(s);
    let s = String.concat(";", List.map(code_of_style, l));
    "\027[" ++ s ++ "m";
  };

  let reset_lit = "\027[0m";

  let color_functions: Format.formatter_tag_functions = (
    {
      mark_open_tag: s =>
        if (get_color_enabled()) {
          ansi_of_tag(s);
        } else {
          "";
        },
      mark_close_tag: _ =>
        if (get_color_enabled()) {
          reset_lit;
        } else {
          "";
        },
      print_open_tag: _ => (),
      print_close_tag: _ => (),
    }: Format.formatter_tag_functions
  );

  let setup = () => {
    Format.pp_set_mark_tags(Format.std_formatter, true);
    Format.pp_set_formatter_tag_functions(
      Format.std_formatter,
      color_functions,
    );
  };
};

module Loc = {
  let print_filename = (ppf, file) =>
    switch (file) {
    /* modified */
    | "_none_"
    | "" => Format.fprintf(ppf, "(No file name)")
    | real_file =>
      Format.fprintf(ppf, "%s", Location.show_filename(real_file))
    };

  let print_loc = (~normalizedRange, ppf, loc: Location.t) => {
    let (file, _, _) = Location.get_pos_info(loc.loc_start);
    if (file == "//toplevel//") {
      if (Location.highlight_locations(ppf, [loc])) {
        ();
      } else {
        Format.fprintf(
          ppf,
          "Characters %i-%i",
          loc.loc_start.pos_cnum,
          loc.loc_end.pos_cnum,
        );
      };
    } else {
      let dim_loc = ppf =>
        fun
        | None => ()
        | Some((
            (start_line, start_line_start_char),
            (end_line, end_line_end_char),
          )) =>
          if (start_line == end_line) {
            if (start_line_start_char == end_line_end_char) {
              Format.fprintf(
                ppf,
                " @{<dim>%i:%i@}",
                start_line,
                start_line_start_char,
              );
            } else {
              Format.fprintf(
                ppf,
                " @{<dim>%i:%i-%i@}",
                start_line,
                start_line_start_char,
                end_line_end_char,
              );
            };
          } else {
            Format.fprintf(
              ppf,
              " @{<dim>%i:%i-%i:%i@}",
              start_line,
              start_line_start_char,
              end_line,
              end_line_end_char,
            );
          };

      Format.fprintf(
        ppf,
        "@{<filename>%a@}%a",
        print_filename,
        file,
        dim_loc,
        normalizedRange,
      );
    };
  };

  let print = (ppf, loc: Location.t) => {
    let (file, start_line, start_char) =
      Location.get_pos_info(loc.loc_start);
    let (_, end_line, end_char) = Location.get_pos_info(loc.loc_end);
    let normalizedRange =
      if (start_char === (-1) || end_char === (-1)) {
        None;
      } else if (start_line == end_line && start_char >= end_char) {
        let same_char = start_char + 1;
        Some(((start_line, same_char), (end_line, same_char)));
      } else {
        Some(((start_line, start_char + 1), (end_line, end_char)));
      };
    Format.fprintf(ppf, "@[%a@]", print_loc(~normalizedRange), loc);
  };
};

let log = x => {
  Format.fprintf(Format.std_formatter, x);
};

let logItem = x => {
  Format.fprintf(Format.std_formatter, "  ");
  Format.fprintf(Format.std_formatter, x);
};

let error = (ppf, s) => Format.fprintf(ppf, "@{<error>%s@}", s);
let info = (ppf, s) => Format.fprintf(ppf, "@{<info>%s@}", s);
let warning = (ppf, s) => Format.fprintf(ppf, "@{<warning>%s@}", s);

let logKind = (body, ~kind, ~loc, ~name) => {
  Format.fprintf(
    Format.std_formatter,
    "@[<v 2>@,%a@,%a@,%a@]@.",
    kind,
    name,
    Loc.print,
    loc,
    body,
    (),
  );
};

let logWarning = (body, ~loc, ~name) =>
  logKind(body, ~kind=warning, ~loc, ~name);
let logInfo = (body, ~loc, ~name) => logKind(body, ~kind=info, ~loc, ~name);
let logError = (body, ~loc, ~name) =>
  logKind(body, ~kind=error, ~loc, ~name);

let readConfig = (~bsVersion, ~getConfigFile, ~getBsConfigFile, ~namespace) => {
  let fromJson = json => {
    let languageString = json |> getString("language");
    let moduleString = json |> getString("module");
    let importPathString = json |> getString("importPath");
    let reasonReactPathString = json |> getString("reasonReactPath");
    let fileHeader = json |> getStringOption("fileHeader");
    let bsBlockPathString = json |> getString("bsBlockPath");
    let bsCurryPathString = json |> getString("bsCurryPath");
    let exportInterfacesBool = json |> getBool("exportInterfaces");
    let generatedFileExtensionStringOption =
      json |> getStringOption("generatedFileExtension");
    let propTypesBool = json |> getBool("propTypes");
    let shimsMap =
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
    let modulesAsObjects = {
      switch (v1) {
      | 5 => bsVersion >= (5, 2, 0)
      | 6 => bsVersion >= (6, 2, 0)
      | _ => v1 > 6
      };
    };
    let recordsAsObjects = {
      switch (v1) {
      | 5 => bsVersion >= (5, 3, 0)
      | 6 => bsVersion >= (6, 3, 0)
      | _ => v1 > 6
      };
    };
    let useUnboxedAnnotations = {
      switch (v1) {
      | 7 => bsVersion > (7, 0, 1)
      | _ => v1 > 7
      };
    };
    if (Debug.config^) {
      logItem("Project root: %s\n", projectRoot^);
      if (bsbProjectRoot^ != projectRoot^) {
        logItem("bsb project root: %s\n", bsbProjectRoot^);
      };
      logItem(
        "Config language:%s module:%s importPath:%s shims:%d entries bsVersion:%d.%d.%d\n",
        languageString,
        moduleString,
        importPathString,
        shimsMap |> ModuleNameMap.cardinal,
        v1,
        v2,
        v3,
      );
    };

    {
      bsBlockPath,
      bsCurryPath,
      bsDependencies: [],
      bsVersion,
      emitCreateBucklescriptBlock: false,
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
      modulesAsObjects,
      namespace: None,
      propTypes,
      reasonReactPath,
      recordsAsObjects,
      shimsMap,
      sources: None,
      useUnboxedAnnotations,
    };
  };

  let fromBsConfig = json =>
    switch (json) {
    | Ext_json_types.Obj({map}) =>
      let config =
        switch (map |> String_map.find_opt("gentypeconfig")) {
        | Some(jsonGenFlowConfig) => jsonGenFlowConfig |> fromJson
        | _ => default
        };
      let config =
        switch (map |> String_map.find_opt("namespace")) {
        | Some(True(_)) => {...config, namespace}
        | _ => config
        };
      let config =
        switch (map |> String_map.find_opt("bs-dependencies")) {
        | Some(Arr({content})) =>
          let strings = ref([]);
          content
          |> Array.iter(x =>
               switch (x) {
               | Ext_json_types.Str({str}) => strings := [str, ...strings^]
               | _ => ()
               }
             );
          {...config, bsDependencies: strings^};
        | _ => config
        };
      let config = {
        switch (map |> String_map.find_opt("sources")) {
        | Some(sourceItem) => {...config, sources: Some(sourceItem)}
        | _ => config
        };
      };
      config;
    | _ => default
    };
  switch (getConfigFile()) {
  | Some(configFile) =>
    try(configFile |> Ext_json_parse.parse_json_from_file |> fromJson) {
    | _ => default
    }
  | None =>
    switch (getBsConfigFile()) {
    | Some(bsConfigFile) =>
      try(bsConfigFile |> Ext_json_parse.parse_json_from_file |> fromBsConfig) {
      | _ => default
      }
    | None => default
    }
  };
};