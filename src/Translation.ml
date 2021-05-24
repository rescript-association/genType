open GenTypeCommon

type t = CodeItem.translation

let empty = ({importTypes = []; codeItems = []; typeDeclarations = []} : t)

let getImportTypeUniqueName ({typeName; asTypeName} : CodeItem.importType) =
  typeName ^ match asTypeName with None -> "" | Some s -> "_as_" ^ s

let importTypeCompare i1 i2 =
  compare (i1 |> getImportTypeUniqueName) (i2 |> getImportTypeUniqueName)

let combine (translations : t list) : t =
  ( translations
  |> List.map (fun {CodeItem.importTypes; codeItems; typeDeclarations} ->
         ((importTypes, codeItems), typeDeclarations))
  |> List.split
  |> fun (x, y) -> (x |> List.split, y) )
  |> fun ((importTypes, codeItems), typeDeclarations) ->
  {
    CodeItem.importTypes = importTypes |> List.concat;
    codeItems = codeItems |> List.concat;
    typeDeclarations = typeDeclarations |> List.concat;
  }

(** Applies type parameters to types (for all) *)
let abstractTheTypeParameters ~typeVars type_ =
  match type_ with
  | Function function_ -> Function {function_ with typeVars}
  | _ -> type_

let depToImportType ~config ~outputFileRelative ~resolver (dep : dep) =
  match dep with
  | _ when dep |> Dependencies.isInternal -> []
  | External name when name = "list" ->
    [
      {
        CodeItem.typeName = "list";
        asTypeName = None;
        importPath =
          ModuleName.reasonPervasives
          |> ModuleResolver.importPathForReasonModuleName ~config
               ~outputFileRelative ~resolver;
      };
    ]
  | External _ -> []
  | Internal _ -> []
  | Dot _ ->
    let moduleName = dep |> Dependencies.getOuterModuleName in
    let typeName =
      dep |> Dependencies.removeExternalOuterModule |> depToString
    in
    let asTypeName =
      match dep |> Dependencies.isInternal with
      | true -> None
      | false -> Some (dep |> depToString)
    in
    let importPath =
      moduleName
      |> ModuleResolver.importPathForReasonModuleName ~config
           ~outputFileRelative ~resolver
    in
    [{typeName; asTypeName; importPath}]

let translateDependencies ~config ~outputFileRelative ~resolver dependencies :
    CodeItem.importType list =
  dependencies
  |> List.map (depToImportType ~config ~outputFileRelative ~resolver)
  |> List.concat

let translateValue ~attributes ~config ~docString ~outputFileRelative ~resolver
    ~typeEnv ~typeExpr ~(addAnnotationsToFunction : type_ -> type_) name : t =
  let nameAs =
    match Annotation.getGenTypeAsRenaming attributes with
    | Some s -> s
    | _ -> name
  in
  let typeExprTranslation =
    typeExpr
    |> TranslateTypeExprFromTypes.translateTypeExprFromTypes ~config ~typeEnv
  in
  let typeVars = typeExprTranslation.type_ |> TypeVars.free in
  let type_ =
    typeExprTranslation.type_
    |> abstractTheTypeParameters ~typeVars
    |> addAnnotationsToFunction
  in
  let resolvedNameOriginal =
    name |> TypeEnv.addModulePath ~typeEnv |> ResolvedName.toString
  in
  let resolvedName = nameAs |> TypeEnv.addModulePath ~typeEnv in
  let moduleAccessPath =
    typeEnv |> TypeEnv.getModuleAccessPath ~name:resolvedNameOriginal
  in
  let codeItems =
    [
      CodeItem.ExportValue
        {docString; moduleAccessPath; originalName = name; resolvedName; type_};
    ]
  in
  {
    importTypes =
      typeExprTranslation.dependencies
      |> translateDependencies ~config ~outputFileRelative ~resolver;
    codeItems;
    typeDeclarations = [];
  }

(**
 * The `make` function is typically of the type:
 *
 *    (~named, ~args=?, 'childrenType) => ReasonReactComponentSpec<
 *      State,
 *      State,
 *      RetainedProps,
 *      RetainedProps,
 *      Action,
 *    >)
 *
 * We take a reference to that function and turn it into a React component of
 * type:
 *
 *
 *     exports.component = (component : React.Component<Props>);
 *
 * Where `Props` is of type:
 *
 *     {named: number, args?: number}
 *)
let translateComponent ~attributes ~config ~docString ~outputFileRelative
    ~resolver ~typeEnv ~typeExpr ~(addAnnotationsToFunction : type_ -> type_)
    name : t =
  let typeExprTranslation_ =
    typeExpr
    |> TranslateTypeExprFromTypes.translateTypeExprFromTypes
         ~config
           (* Only get the dependencies for the prop types.
              The return type is a ReasonReact component. *)
         ~noFunctionReturnDependencies:true ~typeEnv
  in
  let typeExprTranslation =
    {
      typeExprTranslation_ with
      type_ = typeExprTranslation_.type_ |> addAnnotationsToFunction;
    }
  in
  let freeTypeVarsSet = typeExprTranslation.type_ |> TypeVars.free_ in

  (* Replace type variables in props/children with any. *)
  let typeVars, type_ =
    ( [],
      typeExprTranslation.type_
      |> TypeVars.substitute ~f:(fun s ->
             if freeTypeVarsSet |> StringSet.mem s then
               Some (mixedOrUnknown ~config)
             else None) )
  in
  match type_ with
  | Function
      ({
         argTypes = propOrChildren :: childrenOrNil;
         retType =
           Ident
             ({
                name =
                  ( "ReasonReact_componentSpec" | "React_componentSpec"
                  | "ReasonReact_component" | "React_component" );
                typeArgs = _state :: _;
              } as id);
         _;
       } as function_)
    when false ->
    let type_ =
      Function {function_ with retType = Ident {id with typeArgs = []}}
    in

    (* Add children?:any to props type *)
    let propsType =
      match childrenOrNil with
      | [] ->
        (* Then we only extracted a function that accepts children, no props *)
        GroupOfLabeledArgs
          [
            {
              mutable_ = Immutable;
              nameJS = "children";
              nameRE = "children";
              optional = Optional;
              type_ = mixedOrUnknown ~config;
            };
          ]
      | {aType = childrenType} :: _ -> (
        (* Then we had both props and children. *)
        match propOrChildren.aType with
        | GroupOfLabeledArgs fields ->
          GroupOfLabeledArgs
            (fields
            @ [
                {
                  mutable_ = Immutable;
                  nameJS = "children";
                  nameRE = "children";
                  optional = Optional;
                  type_ = childrenType;
                };
              ])
        | t -> t)
    in
    let resolvedTypeName = "Props" |> TypeEnv.addModulePath ~typeEnv in
    let nestedModuleName = typeEnv |> TypeEnv.getNestedModuleName in
    let moduleAccessPath =
      typeEnv |> TypeEnv.getModuleAccessPath ~name:"make"
    in
    let componentAccessPath =
      typeEnv |> TypeEnv.getModuleAccessPath ~component:true ~name:"component"
    in
    let codeItems =
      [
        CodeItem.ExportComponent
          {
            componentAccessPath;
            exportType =
              {
                loc = Location.none;
                nameAs = None;
                opaque = Some false;
                type_ = propsType;
                typeVars;
                resolvedTypeName;
              };
            moduleAccessPath;
            nestedModuleName;
            type_;
          };
      ]
    in
    {
      importTypes =
        typeExprTranslation.dependencies
        |> translateDependencies ~config ~outputFileRelative ~resolver;
      codeItems;
      typeDeclarations = [];
    }
  | _ ->
    (* not a component: treat make as a normal function *)
    name
    |> translateValue ~attributes ~config ~docString ~outputFileRelative
         ~resolver ~typeEnv ~typeExpr ~addAnnotationsToFunction

(**
 [@genType]
 [@bs.module] external myBanner : ReasonReact.reactClass = "./MyBanner";
*)
let translatePrimitive ~config ~outputFileRelative ~resolver ~typeEnv
    (valueDescription : Typedtree.value_description) : t =
  if !Debug.translation then Log_.item "Translate Primitive\n";
  let valueName =
    match valueDescription.val_prim with
    | "" :: _ | [] -> valueDescription.val_id |> Ident.name
    | nameOfExtern :: _ ->
      (* extern foo : someType = "abc"
         The first element of val_prim is "abc" *)
      nameOfExtern
  in
  let typeExprTranslation =
    valueDescription.val_desc
    |> TranslateCoreType.translateCoreType ~config ~typeEnv
  in
  let attributeImport, attributeRenaming =
    valueDescription.val_attributes |> Annotation.getAttributeImportRenaming
  in
  match (typeExprTranslation.type_, attributeImport) with
  | ( Function
        {
          argTypes = _ :: _;
          retType =
            Ident
              {
                name =
                  ( "ReasonReact_componentSpec" | "React_componentSpec"
                  | "ReasonReact_component" | "React_component" );
                typeArgs = _state :: _;
              };
          _;
        },
      Some importString )
    when valueName = "make" && false ->
    let asPath =
      match attributeRenaming with Some asPath -> asPath | None -> ""
    in
    (* Only get the dependencies for the prop types.
       The return type is a ReasonReact component. *)
    let typeExprTranslation =
      valueDescription.val_desc
      |> TranslateCoreType.translateCoreType ~config
           ~noFunctionReturnDependencies:true ~typeEnv
    in
    let freeTypeVarsSet = typeExprTranslation.type_ |> TypeVars.free_ in

    (* Replace type variables in props/children with any. *)
    let typeVars, type_ =
      ( [],
        typeExprTranslation.type_
        |> TypeVars.substitute ~f:(fun s ->
               if freeTypeVarsSet |> StringSet.mem s then
                 Some (mixedOrUnknown ~config)
               else None) )
    in
    let propsFields, childrenTyp =
      match type_ with
      | Function {argTypes = propOrChildren :: childrenOrNil} -> (
        match childrenOrNil with
        | [] -> ([], mixedOrUnknown ~config)
        | {aType = children} :: _ -> (
          match propOrChildren with
          | {aType = GroupOfLabeledArgs fields} ->
            ( fields
              |> List.map (fun ({optional; type_} as field) ->
                     match (type_, optional) with
                     | Option type1, Optional ->
                       {field with optional = Optional; type_ = type1}
                     | _ -> field),
              children )
          | _ -> ([], mixedOrUnknown ~config)))
      | _ -> ([], mixedOrUnknown ~config)
    in
    let propsTyp = Object (Closed, propsFields) in
    let resolvedTypeName = "Props" |> TypeEnv.addModulePath ~typeEnv in
    let propsTypeName = resolvedTypeName |> ResolvedName.toString in
    let codeItems =
      [
        CodeItem.ImportComponent
          {
            asPath;
            childrenTyp;
            exportType =
              {
                loc = Location.none;
                nameAs = None;
                opaque = Some false;
                type_ = propsTyp;
                typeVars;
                resolvedTypeName;
              };
            importAnnotation = importString |> Annotation.importFromString;
            propsFields;
            propsTypeName;
          };
      ]
    in
    {
      importTypes =
        typeExprTranslation.dependencies
        |> translateDependencies ~config ~outputFileRelative ~resolver;
      codeItems;
      typeDeclarations = [];
    }
  | _, Some importString ->
    let asPath =
      match attributeRenaming with Some asPath -> asPath | None -> valueName
    in
    let typeVars = typeExprTranslation.type_ |> TypeVars.free in
    let type_ =
      typeExprTranslation.type_ |> abstractTheTypeParameters ~typeVars
    in
    {
      importTypes =
        typeExprTranslation.dependencies
        |> translateDependencies ~config ~outputFileRelative ~resolver;
      codeItems =
        [
          ImportValue
            {
              asPath;
              importAnnotation = importString |> Annotation.importFromString;
              type_;
              valueName;
            };
        ];
      typeDeclarations = [];
    }
  | _ -> {importTypes = []; codeItems = []; typeDeclarations = []}

let addTypeDeclarationsFromModuleEquations ~typeEnv (translation : t) =
  let eqs = typeEnv |> TypeEnv.getModuleEquations in
  let newTypeDeclarations =
    translation.typeDeclarations
    |> List.map (fun (typeDeclaration : CodeItem.typeDeclaration) ->
           let exportType =
             typeDeclaration.exportFromTypeDeclaration.exportType
           in
           let equations =
             exportType.resolvedTypeName |> ResolvedName.applyEquations ~eqs
           in
           equations
           |> List.map (fun (x, y) ->
                  let newExportType =
                    {
                      exportType with
                      nameAs = None;
                      type_ =
                        y |> ResolvedName.toString
                        |> ident ~builtin:false
                             ~typeArgs:
                               (exportType.typeVars
                               |> List.map (fun s -> TypeVar s));
                      resolvedTypeName = x;
                    }
                  in
                  {
                    CodeItem.exportFromTypeDeclaration =
                      {
                        CodeItem.exportType = newExportType;
                        annotation =
                          typeDeclaration.exportFromTypeDeclaration.annotation;
                      };
                    importTypes = [];
                  }))
    |> List.concat
  in
  match newTypeDeclarations = [] with
  | true -> translation
  | false ->
    {
      translation with
      typeDeclarations = translation.typeDeclarations @ newTypeDeclarations;
    }
