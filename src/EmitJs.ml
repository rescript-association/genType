open GenTypeCommon

type env = {
  requiresEarly : (ImportPath.t * bool) ModuleNameMap.t;
  requires : (ImportPath.t * bool) ModuleNameMap.t;
      (**  For each .cmt we import types from, keep the map of exported types *)
  cmtToExportTypeMap : CodeItem.exportTypeMap StringMap.t;
      (**  Map of types imported from other files *)
  exportTypeMapFromOtherFiles : CodeItem.exportTypeMap;
  importedValueOrComponent : bool;
}

let requireModule ~import ~env ~importPath ?(strict = false) moduleName =
  let requires =
    match import with true -> env.requiresEarly | false -> env.requires
  in
  let requiresNew =
    requires |> ModuleNameMap.add moduleName (importPath, strict)
  in
  match import with
  | true -> {env with requiresEarly = requiresNew}
  | false -> {env with requires = requiresNew}

let createExportTypeMap ~config ~file ~fromCmtReadRecursively
    (typeDeclarations : CodeItem.typeDeclaration list) : CodeItem.exportTypeMap
    =
  if !Debug.codeItems then Log_.item "Create Type Map for %s\n" file;
  let updateExportTypeMap (exportTypeMap : CodeItem.exportTypeMap)
      (typeDeclaration : CodeItem.typeDeclaration) : CodeItem.exportTypeMap =
    let addExportType ~annotation
        ({resolvedTypeName; type_; typeVars} : CodeItem.exportType) =
      let annotation =
        match annotation with
        | Annotation.NoGenType when fromCmtReadRecursively -> Annotation.GenType
        | _ -> annotation
      in
      if !Debug.codeItems then
        Log_.item "Type Map: %s%s%s\n"
          (resolvedTypeName |> ResolvedName.toString)
          (match typeVars = [] with
          | true -> ""
          | false -> "(" ^ (typeVars |> String.concat ",") ^ ")")
          (" "
          ^ (annotation |> Annotation.toString |> EmitText.comment)
          ^ " = "
          ^ (type_
            |> EmitType.typeToString ~config ~typeNameIsInterface:(fun _ ->
                   false)));
      exportTypeMap
      |> StringMap.add
           (resolvedTypeName |> ResolvedName.toString)
           {CodeItem.typeVars; type_; annotation}
    in
    match typeDeclaration.exportFromTypeDeclaration with
    | {exportType; annotation} -> exportType |> addExportType ~annotation
  in
  typeDeclarations |> List.fold_left updateExportTypeMap StringMap.empty

let codeItemToString ~config ~typeNameIsInterface (codeItem : CodeItem.t) =
  match codeItem with
  | ExportValue {resolvedName; type_} ->
    "ExportValue" ^ " resolvedName:"
    ^ ResolvedName.toString resolvedName
    ^ " type:"
    ^ EmitType.typeToString ~config ~typeNameIsInterface type_
  | ImportValue {importAnnotation} ->
    "ImportValue " ^ (importAnnotation.importPath |> ImportPath.dump)

let emitExportType ?early ~emitters ~config ~typeGetNormalized
    ~typeNameIsInterface
    {CodeItem.loc; nameAs; opaque; type_; typeVars; resolvedTypeName} =
  let freeTypeVars = TypeVars.free type_ in
  let isGADT =
    freeTypeVars |> List.exists (fun s -> not (List.mem s typeVars))
  in
  let opaque =
    match opaque with
    | Some true -> opaque
    | _ when isGADT ->
      Log_.Color.setup ();
      Log_.info ~loc ~name:"Warning genType" (fun ppf () ->
          Format.fprintf ppf
            "GADT types are not supported: exporting %s as opaque type"
            (resolvedTypeName |> ResolvedName.toString));
      Some true
    | _ -> opaque
  in
  let opaque, type_ =
    match opaque with
    | Some opaque -> (opaque, type_)
    | None ->
      let normalized = type_ |> typeGetNormalized in
      (false, normalized)
  in
  resolvedTypeName |> ResolvedName.toString
  |> EmitType.emitExportType ?early ~config ~emitters ~nameAs ~opaque ~type_
       ~typeNameIsInterface ~typeVars

let typeNameIsInterface ~(exportTypeMap : CodeItem.exportTypeMap)
    ~(exportTypeMapFromOtherFiles : CodeItem.exportTypeMap) typeName =
  let typeIsInterface type_ =
    match type_ with Object _ | Record _ -> true | _ -> false
  in
  match exportTypeMap |> StringMap.find typeName with
  | {type_} -> type_ |> typeIsInterface
  | exception Not_found -> (
    match exportTypeMapFromOtherFiles |> StringMap.find typeName with
    | {type_} -> type_ |> typeIsInterface
    | exception Not_found -> false)

let emitExportFromTypeDeclaration ~config ~emitters ~typeGetNormalized ~env
    ~typeNameIsInterface
    (exportFromTypeDeclaration : CodeItem.exportFromTypeDeclaration) =
  ( env,
    exportFromTypeDeclaration.exportType
    |> emitExportType ~emitters ~config ~typeGetNormalized ~typeNameIsInterface
  )

let emitExportFromTypeDeclarations ~config ~emitters ~env ~typeGetNormalized
    ~typeNameIsInterface exportFromTypeDeclarations =
  exportFromTypeDeclarations
  |> List.fold_left
       (fun (env, emitters) ->
         emitExportFromTypeDeclaration ~config ~emitters ~env ~typeGetNormalized
           ~typeNameIsInterface)
       (env, emitters)

let rec emitCodeItem ~config ~emitters ~moduleItemsEmitter ~env ~fileName
    ~outputFileRelative ~resolver ~typeGetConverter ~typeGetInlined
    ~typeGetNormalized ~typeNameIsInterface ~variantTables codeItem =
  let language = config.language in
  if !Debug.codeItems then
    Log_.item "Code Item: %s\n"
      (codeItem |> codeItemToString ~config ~typeNameIsInterface);
  let indent = Some "" in
  match codeItem with
  | ImportValue {asPath; importAnnotation; type_; valueName} ->
    let nameGen = EmitText.newNameGen () in
    let importPath = importAnnotation.importPath in
    let importFile = importAnnotation.name in
    let firstNameInPath, restOfPath =
      match valueName = asPath with
      | true -> (valueName, "")
      | false -> (
        match asPath |> Str.split (Str.regexp "\\.") with
        | x :: y -> (x, "" :: y |> String.concat ".")
        | _ -> (asPath, ""))
    in
    let importFileVariable = "$$" ^ importFile in
    let emitters, importedAsName, env =
      match (language, config.module_) with
      | _, ES6 | TypeScript, _ ->
        (* emit an import {... as ...} immediately *)
        let valueNameNotChecked = valueName ^ "NotChecked" in
        let emitters =
          importPath
          |> EmitType.emitImportValueAsEarly ~config ~emitters
               ~name:firstNameInPath ~nameAs:(Some valueNameNotChecked)
        in
        (emitters, valueNameNotChecked, env)
      | (Flow | Untyped), _ ->
        (* add an early require(...) *)
        let importedAsName =
          match firstNameInPath = "default" with
          | true -> importFileVariable
          | false -> importFileVariable ^ "." ^ firstNameInPath
        in
        let env =
          importFileVariable |> ModuleName.fromStringUnsafe
          |> requireModule ~import:true ~env ~importPath ~strict:true
        in
        (emitters, importedAsName, env)
    in
    let type_ =
      match type_ with
      | Function
          ({argTypes = [{aType = Object (_, fields)}]; retType} as function_)
        when retType |> EmitType.isTypeFunctionComponent ~config ~fields ->
        let componentName =
          match importFile with "." | ".." -> None | _ -> Some importFile
        in
        Function {function_ with componentName}
      | _ -> type_
    in
    let converter = type_ |> typeGetConverter in
    let valueNameTypeChecked = valueName ^ "TypeChecked" in
    let emitters =
      (importedAsName ^ restOfPath) ^ ";"
      |> EmitType.emitExportConstEarly ~config
           ~comment:
             ("In case of type error, check the type of '" ^ valueName
            ^ "' in '"
             ^ (fileName |> ModuleName.toString)
             ^ ".re'" ^ " and '"
             ^ (importPath |> ImportPath.emit ~config)
             ^ "'.")
           ~emitters ~name:valueNameTypeChecked ~type_ ~typeNameIsInterface
    in
    let valueNameNotDefault =
      match valueName = "default" with
      | true -> Runtime.default
      | false -> valueName
    in
    let emitters =
      (valueNameTypeChecked
      |> Converter.toReason ~config ~converter ~indent ~nameGen ~variantTables
      |> EmitType.emitTypeCast ~config ~type_ ~typeNameIsInterface)
      ^ ";"
      |> EmitType.emitExportConstEarly
           ~comment:
             ("Export '" ^ valueNameNotDefault
            ^ "' early to allow circular import from the '.bs.js' file.")
           ~config ~emitters ~name:valueNameNotDefault
           ~type_:(mixedOrUnknown ~config) ~typeNameIsInterface
    in
    let emitters =
      match valueName = "default" with
      | true -> EmitType.emitExportDefault ~emitters ~config valueNameNotDefault
      | false -> emitters
    in
    ({env with importedValueOrComponent = true}, emitters)
  | ExportValue {docString; moduleAccessPath; originalName; resolvedName; type_}
    ->
    let resolvedNameStr = ResolvedName.toString resolvedName in
    let nameGen = EmitText.newNameGen () in
    let importPath =
      fileName
      |> ModuleResolver.resolveModule ~importExtension:config.suffix
           ~outputFileRelative ~resolver ~useBsDependencies:false
    in
    let fileNameBs = fileName |> ModuleName.forBsFile in
    let envWithRequires =
      fileNameBs |> requireModule ~import:false ~env ~importPath
    in
    let default = "default" in
    let make = "make" in
    let name =
      match originalName = default with
      | true -> Runtime.default
      | false -> resolvedNameStr
    in
    let module HookType = struct
      type t = {
        propsType : type_;
        resolvedTypeName : ResolvedName.t;
        retType : type_;
        typeVars : string list;
      }
    end in
    let type_, hookType =
      match type_ with
      | Function
          ({
             argTypes = [{aType = Object (closedFlags, fields)}];
             retType;
             typeVars;
           } as function_)
        when retType |> EmitType.isTypeFunctionComponent ~config ~fields ->
        let propsType = Object (closedFlags, fields) in
        let function_ =
          {function_ with argTypes = [{aName = ""; aType = propsType}]}
        in
        let chopSuffix suffix =
          match resolvedNameStr = suffix with
          | true -> ""
          | false -> (
            match Filename.check_suffix resolvedNameStr ("_" ^ suffix) with
            | true -> Filename.chop_suffix resolvedNameStr ("_" ^ suffix)
            | false -> resolvedNameStr)
        in
        let suffix =
          if originalName = default then chopSuffix default
          else if originalName = make then chopSuffix make
          else resolvedNameStr
        in
        let hookName =
          (fileName |> ModuleName.toString)
          ^ match suffix = "" with true -> suffix | false -> "_" ^ suffix
        in
        let resolvedTypeName =
          if
            (not config.emitTypePropDone)
            && (originalName = default || originalName = make)
          then (
            config.emitTypePropDone <- true;
            ResolvedName.fromString "Props")
          else ResolvedName.fromString name |> ResolvedName.dot "Props"
        in
        ( Function {function_ with componentName = Some hookName},
          Some {HookType.propsType; resolvedTypeName; retType; typeVars} )
      | _ -> (type_, None)
    in

    let converter = type_ |> typeGetConverter in
    resolvedName
    |> ExportModule.extendExportModules ~converter ~moduleItemsEmitter ~type_;
    let emitters =
      match hookType with
      | Some {propsType; resolvedTypeName; typeVars} ->
        let exportType =
          ({
             loc = Location.none;
             nameAs = None;
             opaque = Some false;
             type_ = propsType;
             typeVars;
             resolvedTypeName;
           }
            : CodeItem.exportType)
        in
        if config.language = TypeScript then
          (* For doc gen (https://github.com/cristianoc/genType/issues/342) *)
          config.emitImportReact <- true;
        emitExportType ~emitters ~config ~typeGetNormalized ~typeNameIsInterface
          exportType
      | _ -> emitters
    in
    let emitters =
      ((fileNameBs |> ModuleName.toString)
       ^ "."
       ^ (moduleAccessPath |> Runtime.emitModuleAccessPath ~config)
      |> Converter.toJS ~config ~converter ~indent ~nameGen ~variantTables)
      ^ ";"
      |> EmitType.emitExportConst ~config ~docString ~emitters ~name ~type_
           ~typeNameIsInterface
    in
    let emitters =
      match hookType with
      | Some {propsType = Object (_, fields)}
        when config.language = Untyped && config.propTypes ->
        fields
        |> List.map (fun (field : field) ->
               let type_ = field.type_ |> typeGetInlined in
               {field with type_})
        |> EmitType.emitPropTypes ~config ~name ~emitters ~indent
      | _ -> emitters
    in
    let emitters =
      match originalName = default with
      | true -> EmitType.emitExportDefault ~emitters ~config Runtime.default
      | false -> emitters
    in
    (envWithRequires, emitters)

and emitCodeItems ~config ~outputFileRelative ~emitters ~moduleItemsEmitter ~env
    ~fileName ~resolver ~typeNameIsInterface ~typeGetConverter ~typeGetInlined
    ~typeGetNormalized ~variantTables codeItems =
  codeItems
  |> List.fold_left
       (fun (env, emitters) ->
         emitCodeItem ~config ~emitters ~moduleItemsEmitter ~env ~fileName
           ~outputFileRelative ~resolver ~typeGetConverter ~typeGetInlined
           ~typeGetNormalized ~typeNameIsInterface ~variantTables)
       (env, emitters)

let emitRequires ~importedValueOrComponent ~early ~config ~requires emitters =
  ModuleNameMap.fold
    (fun moduleName (importPath, strict) emitters ->
      importPath
      |> EmitType.emitRequire ~importedValueOrComponent ~early ~emitters ~config
           ~moduleName ~strict)
    requires emitters

let emitVariantTables ~config ~emitters variantTables =
  let typeAnnotation =
    match config.language = TypeScript with
    | true -> ": { [key: string]: any }"
    | false -> ""
  in
  let emitTable ~table ~toJS (variantC : Converter.variantC) =
    "const " ^ table ^ typeAnnotation ^ " = {"
    ^ (variantC.noPayloads
      |> List.map (fun case ->
             let js = case |> labelJSToString ~alwaysQuotes:(not toJS) in
             let re =
               case.label
               |> Runtime.emitVariantLabel ~polymorphic:variantC.polymorphic
             in
             match toJS with
             | true -> (re |> EmitText.quotesIfRequired) ^ ": " ^ js
             | false -> js ^ ": " ^ re)
      |> String.concat ", ")
    ^ "};"
  in
  Hashtbl.fold
    (fun (_, toJS) variantC l -> (variantC, toJS) :: l)
    variantTables []
  |> List.sort (fun (variantC1, toJS1) (variantC2, toJS2) ->
         let n = compare variantC1.Converter.hash variantC2.hash in
         match n <> 0 with true -> n | false -> compare toJS2 toJS1)
  |> List.fold_left
       (fun emitters (variantC, toJS) ->
         variantC
         |> emitTable
              ~table:(variantC.Converter.hash |> variantTable ~toJS)
              ~toJS
         |> Emitters.requireEarly ~emitters)
       emitters

let typeGetInlined ~config ~exportTypeMap type_ =
  type_
  |> Converter.typeGetNormalized ~config ~inline:true
       ~lookupId:(fun s -> exportTypeMap |> StringMap.find s)
       ~typeNameIsInterface:(fun _ -> false)

(** Read the cmt file referenced in an import type,
   and recursively for the import types obtained from reading the cmt file. *)
let rec readCmtFilesRecursively ~config ~env ~inputCmtTranslateTypeDeclarations
    ~outputFileRelative ~resolver {CodeItem.typeName; asTypeName; importPath} =
  let updateTypeMapFromOtherFiles ~asType ~exportTypeMapFromCmt env =
    match exportTypeMapFromCmt |> StringMap.find typeName with
    | (exportTypeItem : CodeItem.exportTypeItem) ->
      let type_ =
        exportTypeItem.type_
        |> typeGetInlined ~config ~exportTypeMap:exportTypeMapFromCmt
      in
      {
        env with
        exportTypeMapFromOtherFiles =
          env.exportTypeMapFromOtherFiles
          |> StringMap.add asType {exportTypeItem with type_};
      }
    | exception Not_found -> env
  in
  let cmtFile =
    importPath
    |> ImportPath.toCmt ~config ~outputFileRelative
    |> Paths.getCmtFile
  in
  match asTypeName with
  | Some asType when cmtFile <> "" -> (
    match env.cmtToExportTypeMap |> StringMap.find cmtFile with
    | exportTypeMapFromCmt ->
      env |> updateTypeMapFromOtherFiles ~asType ~exportTypeMapFromCmt
    | exception Not_found ->
      (* cmt file not read before: this ensures termination  *)
      let typeDeclarations =
        Cmt_format.read_cmt cmtFile
        |> inputCmtTranslateTypeDeclarations ~config ~outputFileRelative
             ~resolver
        |> fun (x : CodeItem.translation) -> x.typeDeclarations
      in
      let exportTypeMapFromCmt =
        typeDeclarations
        |> createExportTypeMap ~config ~fromCmtReadRecursively:true
             ~file:(cmtFile |> Filename.basename |> Filename.chop_extension)
      in
      let cmtToExportTypeMap =
        env.cmtToExportTypeMap |> StringMap.add cmtFile exportTypeMapFromCmt
      in
      let env =
        {env with cmtToExportTypeMap}
        |> updateTypeMapFromOtherFiles ~asType ~exportTypeMapFromCmt
      in
      let newImportTypes =
        typeDeclarations
        |> List.map (fun (typeDeclaration : CodeItem.typeDeclaration) ->
               typeDeclaration.importTypes)
        |> List.concat
      in
      newImportTypes
      |> List.fold_left
           (fun env newImportType ->
             newImportType
             |> readCmtFilesRecursively ~config ~env
                  ~inputCmtTranslateTypeDeclarations ~outputFileRelative
                  ~resolver)
           env)
  | _ -> env

let emitImportType ~config ~emitters ~env ~inputCmtTranslateTypeDeclarations
    ~outputFileRelative ~resolver ~typeNameIsInterface
    ({CodeItem.typeName; asTypeName; importPath} as importType) =
  let env =
    importType
    |> readCmtFilesRecursively ~config ~env ~inputCmtTranslateTypeDeclarations
         ~outputFileRelative ~resolver
  in
  let emitters =
    EmitType.emitImportTypeAs ~emitters ~config ~typeName ~asTypeName
      ~typeNameIsInterface:(typeNameIsInterface ~env) ~importPath
  in
  (env, emitters)

let emitImportTypes ~config ~emitters ~env ~inputCmtTranslateTypeDeclarations
    ~outputFileRelative ~resolver ~typeNameIsInterface importTypes =
  importTypes
  |> List.fold_left
       (fun (env, emitters) ->
         emitImportType ~config ~emitters ~env
           ~inputCmtTranslateTypeDeclarations ~outputFileRelative ~resolver
           ~typeNameIsInterface)
       (env, emitters)

let getAnnotatedTypedDeclarations ~annotatedSet typeDeclarations =
  typeDeclarations
  |> List.map (fun typeDeclaration ->
         let nameInAnnotatedSet =
           annotatedSet
           |> StringSet.mem
                (typeDeclaration.CodeItem.exportFromTypeDeclaration.exportType
                   .resolvedTypeName |> ResolvedName.toString)
         in
         if nameInAnnotatedSet then
           {
             typeDeclaration with
             exportFromTypeDeclaration =
               {
                 typeDeclaration.exportFromTypeDeclaration with
                 annotation = GenType;
               };
           }
         else typeDeclaration)
  |> List.filter
       (fun
         ({exportFromTypeDeclaration = {annotation}} : CodeItem.typeDeclaration)
       -> annotation <> NoGenType)

let propagateAnnotationToSubTypes ~codeItems (typeMap : CodeItem.exportTypeMap)
    =
  let annotatedSet = ref StringSet.empty in
  let initialAnnotatedTypes =
    typeMap |> StringMap.bindings
    |> List.filter (fun (_, {CodeItem.annotation}) ->
           annotation = Annotation.GenType)
    |> List.map (fun (_, {CodeItem.type_}) -> type_)
  in
  let typesOfExportedValue (codeItem : CodeItem.t) =
    match codeItem with ExportValue {type_} | ImportValue {type_} -> [type_]
  in
  let typesOfExportedValues =
    codeItems |> List.map typesOfExportedValue |> List.concat
  in
  let visitTypAndUpdateMarked type0 =
    let visited = ref StringSet.empty in
    let rec visit type_ =
      match type_ with
      | Ident {name = typeName; typeArgs} ->
        if !visited |> StringSet.mem typeName then ()
        else (
          visited := !visited |> StringSet.add typeName;
          typeArgs |> List.iter visit;
          match typeMap |> StringMap.find typeName with
          | {annotation = GenType | GenTypeOpaque} -> ()
          | {type_ = type1; annotation = NoGenType} ->
            if !Debug.translation then
              Log_.item "Marking Type As Annotated %s\n" typeName;
            annotatedSet := !annotatedSet |> StringSet.add typeName;
            type1 |> visit
          | exception Not_found ->
            annotatedSet := !annotatedSet |> StringSet.add typeName)
      | Array (t, _) -> t |> visit
      | Function {argTypes; retType} ->
        argTypes |> List.iter (fun {aType} -> visit aType);
        retType |> visit
      | GroupOfLabeledArgs fields | Object (_, fields) | Record fields ->
        fields |> List.iter (fun {type_} -> type_ |> visit)
      | Option t | Null t | Nullable t | Promise t -> t |> visit
      | Tuple innerTypes -> innerTypes |> List.iter visit
      | TypeVar _ -> ()
      | Variant {inherits; payloads} ->
        inherits |> List.iter visit;
        payloads |> List.iter (fun {t} -> t |> visit)
    in
    type0 |> visit
  in
  initialAnnotatedTypes @ typesOfExportedValues
  |> List.iter visitTypAndUpdateMarked;
  let newTypeMap =
    typeMap
    |> StringMap.mapi
         (fun typeName (exportTypeItem : CodeItem.exportTypeItem) ->
           {
             exportTypeItem with
             annotation =
               (match !annotatedSet |> StringSet.mem typeName with
               | true -> Annotation.GenType
               | false -> exportTypeItem.annotation);
           })
  in
  (newTypeMap, !annotatedSet)

let emitTranslationAsString ~config ~fileName ~inputCmtTranslateTypeDeclarations
    ~outputFileRelative ~resolver (translation : Translation.t) =
  let initialEnv =
    {
      requires = ModuleNameMap.empty;
      requiresEarly = ModuleNameMap.empty;
      cmtToExportTypeMap = StringMap.empty;
      exportTypeMapFromOtherFiles = StringMap.empty;
      importedValueOrComponent = false;
    }
  in
  let variantTables = Hashtbl.create 1 in
  let exportTypeMap, annotatedSet =
    translation.typeDeclarations
    |> createExportTypeMap ~config
         ~file:(fileName |> ModuleName.toString)
         ~fromCmtReadRecursively:false
    |> propagateAnnotationToSubTypes ~codeItems:translation.codeItems
  in
  let annotatedTypeDeclarations =
    translation.typeDeclarations |> getAnnotatedTypedDeclarations ~annotatedSet
  in
  let importTypesFromTypeDeclarations =
    annotatedTypeDeclarations
    |> List.map (fun (typeDeclaration : CodeItem.typeDeclaration) ->
           typeDeclaration.importTypes)
    |> List.concat
  in
  let exportFromTypeDeclarations =
    annotatedTypeDeclarations
    |> List.map (fun (typeDeclaration : CodeItem.typeDeclaration) ->
           typeDeclaration.exportFromTypeDeclaration)
  in
  let typeNameIsInterface ~env =
    typeNameIsInterface ~exportTypeMap
      ~exportTypeMapFromOtherFiles:env.exportTypeMapFromOtherFiles
  in
  let lookupId_ ~env s =
    try exportTypeMap |> StringMap.find s
    with Not_found -> env.exportTypeMapFromOtherFiles |> StringMap.find s
  in
  let typeGetNormalized_ ~env ?(inline = false) type_ =
    type_
    |> Converter.typeGetNormalized ~config ~inline ~lookupId:(lookupId_ ~env)
         ~typeNameIsInterface:(typeNameIsInterface ~env)
  in
  let typeGetConverter_ ~env type_ =
    type_
    |> Converter.typeGetConverter ~config ~lookupId:(lookupId_ ~env)
         ~typeNameIsInterface:(typeNameIsInterface ~env)
  in
  let emitters = Emitters.initial
  and moduleItemsEmitter = ExportModule.createModuleItemsEmitter ()
  and env = initialEnv in
  let env, emitters =
    (* imports from type declarations go first to build up type tables *)
    importTypesFromTypeDeclarations @ translation.importTypes
    |> List.sort_uniq Translation.importTypeCompare
    |> emitImportTypes ~config ~emitters ~env ~inputCmtTranslateTypeDeclarations
         ~outputFileRelative ~resolver ~typeNameIsInterface
  in
  let env, emitters =
    exportFromTypeDeclarations
    |> emitExportFromTypeDeclarations ~config ~emitters
         ~typeGetNormalized:(typeGetNormalized_ ~env) ~env
         ~typeNameIsInterface:(typeNameIsInterface ~env)
  in
  let env, emitters =
    translation.codeItems
    |> emitCodeItems ~config ~emitters ~moduleItemsEmitter ~env ~fileName
         ~outputFileRelative ~resolver
         ~typeGetInlined:(typeGetNormalized_ ~env ~inline:true)
         ~typeGetNormalized:(typeGetNormalized_ ~env)
         ~typeGetConverter:(typeGetConverter_ ~env)
         ~typeNameIsInterface:(typeNameIsInterface ~env) ~variantTables
  in
  let emitters =
    match config.emitImportReact with
    | true -> EmitType.emitImportReact ~emitters ~config
    | false -> emitters
  in
  let env =
    match config.emitImportCurry with
    | true ->
      ModuleName.curry
      |> requireModule ~import:true ~env
           ~importPath:(ImportPath.bsCurryPath ~config)
    | false -> env
  in
  let finalEnv =
    match config.emitImportPropTypes with
    | true ->
      ModuleName.propTypes
      |> requireModule ~import:true ~env ~importPath:ImportPath.propTypes
    | false -> env
  in
  let emitters = variantTables |> emitVariantTables ~config ~emitters in
  let emitters =
    moduleItemsEmitter
    |> ExportModule.emitAllModuleItems ~config ~emitters ~fileName
  in
  emitters
  |> emitRequires ~importedValueOrComponent:false ~early:true ~config
       ~requires:finalEnv.requiresEarly
  |> emitRequires ~importedValueOrComponent:finalEnv.importedValueOrComponent
       ~early:false ~config ~requires:finalEnv.requires
  |> Emitters.toString ~separator:"\n\n"
