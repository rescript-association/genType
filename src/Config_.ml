module ModuleNameMap = Map.Make (ModuleName)

let bsbProjectRoot = ref ""
let projectRoot = ref ""

type language = Flow | TypeScript | Untyped
type module_ = CommonJS | ES6
type importPath = Relative | Node
type bsVersion = int * int * int

type config = {
  bsCurryPath : string option;
  bsDependencies : string list;
  mutable emitFlowAny : bool;
  mutable emitImportCurry : bool;
  mutable emitImportPropTypes : bool;
  mutable emitImportReact : bool;
  mutable emitTypePropDone : bool;
  exportInterfaces : bool;
  generatedFileExtension : string option;
  importPath : importPath;
  language : language;
  module_ : module_;
  namespace : string option;
  platformLib : string;
  shimsMap : ModuleName.t ModuleNameMap.t;
  sources : Ext_json_types.t option;
  suffix : string;
}

let default =
  {
    bsCurryPath = None;
    bsDependencies = [];
    emitFlowAny = false;
    emitImportCurry = false;
    emitImportPropTypes = false;
    emitImportReact = false;
    emitTypePropDone = false;
    exportInterfaces = false;
    generatedFileExtension = None;
    importPath = Relative;
    language = Flow;
    module_ = ES6;
    namespace = None;
    platformLib = "";
    shimsMap = ModuleNameMap.empty;
    sources = None;
    suffix = "";
  }

let bsPlatformLib ~config =
  match config.module_ with
  | ES6 -> config.platformLib ^ "/lib/es6"
  | CommonJS -> config.platformLib ^ "/lib/js"

let bsPlatformLibExtension = ".js"

let getBsCurryPath ~config =
  match config.bsCurryPath with
  | None -> bsPlatformLib ~config ^ "/curry" ^ bsPlatformLibExtension
  | Some s -> s

type map = Ext_json_types.t String_map.t

let getOpt s (map : map) = String_map.find_opt s map

let getBool s map =
  match map |> getOpt s with
  | Some (True _) -> Some true
  | Some (False _) -> Some false
  | _ -> None

let getString s map =
  match map |> getOpt s with Some (Str {str}) -> str | _ -> ""

let getStringOption s map =
  match map |> getOpt s with Some (Str {str}) -> Some str | _ -> None

let getShims map =
  let shims = ref [] in
  (match map |> getOpt "shims" with
  | Some (Obj {map = shimsMap}) ->
    shimsMap
    |> String_map.iter (fun fromModule toModule ->
           match toModule with
           | Ext_json_types.Str {str} -> shims := (fromModule, str) :: !shims
           | _ -> ())
  | Some (Arr {content}) ->
    (* To be deprecated: array of strings *)
    content
    |> Array.iter (fun x ->
           match x with
           | Ext_json_types.Str {str} ->
             let fromTo = Str.split (Str.regexp "=") str |> Array.of_list in
             assert (Array.length fromTo == 2);
             shims := (fromTo.(0), fromTo.(1)) :: !shims
           | _ -> ())
  | _ -> ());
  !shims

let setDebug ~gtconf =
  match gtconf |> getOpt "debug" with
  | Some (Obj {map}) -> map |> String_map.iter Debug.setItem
  | _ -> ()

let readConfig ~bsVersion ~getBsConfigFile ~namespace =
  let parseConfig ~bsconf ~gtconf =
    let languageString = gtconf |> getString "language" in
    let moduleString = gtconf |> getStringOption "module" in
    let importPathString = gtconf |> getString "importPath" in
    let bsCurryPathString = gtconf |> getString "bsCurryPath" in
    let exportInterfacesBool = gtconf |> getBool "exportInterfaces" in
    let generatedFileExtensionStringOption =
      gtconf |> getStringOption "generatedFileExtension"
    in
    let shimsMap =
      gtconf |> getShims
      |> List.fold_left
           (fun map (fromModule, toModule) ->
             let moduleName =
               (fromModule |> ModuleName.fromStringUnsafe : ModuleName.t)
             in
             let shimModuleName = toModule |> ModuleName.fromStringUnsafe in
             ModuleNameMap.add moduleName shimModuleName map)
           ModuleNameMap.empty
    in
    setDebug ~gtconf;
    let language =
      match languageString with
      | "typescript" -> TypeScript
      | "untyped" -> Untyped
      | _ -> Flow
    in
    let module_ =
      let packageSpecsModuleString =
        match bsconf |> getOpt "package-specs" with
        | Some (Obj {map = packageSpecs}) ->
          packageSpecs |> getStringOption "module"
        | _ -> None
      in
      (* Give priority to gentypeconfig, followed by package-specs *)
      match (moduleString, packageSpecsModuleString) with
      | Some "commonjs", _ -> CommonJS
      | Some "es6", _ -> ES6
      | None, Some "commonjs" -> CommonJS
      | None, Some ("es6" | "es6-global") -> ES6
      | _ -> default.module_
    in
    let importPath =
      match importPathString with
      | "relative" -> Relative
      | "node" -> Node
      | _ -> default.importPath
    in
    let bsCurryPath =
      match bsCurryPathString with "" -> None | _ -> Some bsCurryPathString
    in
    let exportInterfaces =
      match exportInterfacesBool with
      | None -> default.exportInterfaces
      | Some b -> b
    in
    let generatedFileExtension = generatedFileExtensionStringOption in
    let bsVersion =
      match bsVersion with
      | None -> (0, 0, 0)
      | Some s -> (
        match s |> Str.split (Str.regexp (Str.quote ".")) with
        | x1 :: x2 :: x3 :: _ ->
          let v1 = int_of_string x1 in
          let v2 = int_of_string x2 in
          let v3 =
            match x3 |> Str.split (Str.regexp "-") with
            | x3 :: _ -> int_of_string x3
            | _ -> 0
          in
          (v1, v2, v3)
        | _ -> (0, 0, 0))
    in
    let externalStdlib = bsconf |> getStringOption "external-stdlib" in
    let v1, v2, v3 = bsVersion in
    let platformLib =
      match externalStdlib with
      | None -> "rescript"
      | Some externalStdlib -> externalStdlib
    in
    if !Debug.config then (
      Log_.item "Project root: %s\n" !projectRoot;
      if !bsbProjectRoot <> !projectRoot then
        Log_.item "bsb project root: %s\n" !bsbProjectRoot;
      Log_.item
        "Config language:%s module:%s importPath:%s shims:%d entries \
         bsVersion:%d.%d.%d\n"
        languageString
        (match moduleString with None -> "" | Some s -> s)
        importPathString
        (shimsMap |> ModuleNameMap.cardinal)
        v1 v2 v3);
    let namespace =
      match bsconf |> getOpt "namespace" with
      | Some (True _) -> namespace
      | _ -> default.namespace
    in
    let suffix =
      match bsconf |> getStringOption "suffix" with
      | Some ".bs.js" -> ".bs"
      | Some s -> s
      | _ -> ".bs"
    in
    let bsDependencies =
      match bsconf |> getOpt "bs-dependencies" with
      | Some (Arr {content}) ->
        let strings = ref [] in
        content
        |> Array.iter (fun x ->
               match x with
               | Ext_json_types.Str {str} -> strings := str :: !strings
               | _ -> ());
        !strings
      | _ -> default.bsDependencies
    in
    let sources =
      match bsconf |> getOpt "sources" with
      | Some sourceItem -> Some sourceItem
      | _ -> default.sources
    in
    {
      bsCurryPath;
      bsDependencies;
      suffix;
      emitFlowAny = false;
      emitImportCurry = false;
      emitImportPropTypes = false;
      emitImportReact = false;
      emitTypePropDone = false;
      exportInterfaces;
      generatedFileExtension;
      importPath;
      language;
      module_;
      namespace;
      platformLib;
      shimsMap;
      sources;
    }
  in
  match getBsConfigFile () with
  | Some bsConfigFile -> (
    try
      let json = bsConfigFile |> Ext_json_parse.parse_json_from_file in
      match json with
      | Obj {map = bsconf} -> (
        match bsconf |> getOpt "gentypeconfig" with
        | Some (Obj {map = gtconf}) -> parseConfig ~bsconf ~gtconf
        | _ -> default)
      | _ -> default
    with _ -> default)
  | None -> default
