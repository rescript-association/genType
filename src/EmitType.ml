open GenTypeCommon

let flowExpectedError =
  "// $FlowExpectedError: Reason checked type sufficiently\n"

let flowTypeAny = flowExpectedError ^ "type $any = any;\n"

let fileHeader ~config ~sourceFile =
  let makeHeader ~lines =
    let lines =
      match config.fileHeader with
      | Some header -> header :: lines
      | None -> lines
    in
    match lines with
    | [line] -> "/* " ^ line ^ " */\n"
    | _ ->
      "/** \n"
      ^ (lines |> List.map (fun line -> " * " ^ line) |> String.concat "\n")
      ^ "\n */\n"
  in
  match config.language with
  | Flow -> (
    makeHeader
      ~lines:["@flow strict"; "@" ^ "generated from " ^ sourceFile; "@nolint"]
    ^ "/* eslint-disable */\n"
    ^ match config.emitFlowAny with true -> flowTypeAny | false -> "")
  | TypeScript ->
    makeHeader
      ~lines:["TypeScript file generated from " ^ sourceFile ^ " by genType."]
    ^ "/* eslint-disable import/first */\n\n"
  | Untyped ->
    makeHeader
      ~lines:["Untyped file generated from " ^ sourceFile ^ " by genType."]
    ^ "/* eslint-disable */\n"

let generatedFilesExtension ~config =
  match config.generatedFileExtension with
  | Some s ->
    (* from .foo.bar to .foo *)
    Filename.remove_extension s
  | None -> ".gen"

let outputFileSuffix ~config =
  match config.generatedFileExtension with
  | Some s when Filename.extension s <> "" (* double extension  *) -> s
  | _ -> (
    match config.language with
    | Flow | Untyped -> generatedFilesExtension ~config ^ ".js"
    | TypeScript -> generatedFilesExtension ~config ^ ".tsx")

let generatedModuleExtension ~config = generatedFilesExtension ~config

let shimExtension ~config =
  match config.language with
  | Flow -> ".shim.js"
  | TypeScript -> ".shim.ts"
  | Untyped -> ".shim.not.used"

let interfaceName ~config name =
  match config.exportInterfaces with true -> "I" ^ name | false -> name

let typeReactComponent ~config ~propsType =
  (match config.language = Flow with
  | true -> "React$ComponentType"
  | false -> "React.ComponentType")
  |> ident ~builtin:true ~typeArgs:[propsType]

let typeReactContext ~config ~type_ =
  (match config.language = Flow with
  | true -> "React$Context"
  | false -> "React.Context")
  |> ident ~builtin:true ~typeArgs:[type_]

let typeReactElementFlow = ident ~builtin:true "React$Node"

let typeReactElementTypeScript = ident ~builtin:true "JSX.Element"

let typeReactChildTypeScript = ident ~builtin:true "React.ReactNode"

let typeReactElement ~config =
  match config.language = Flow with
  | true -> typeReactElementFlow
  | false -> typeReactElementTypeScript

let typeReactChild ~config =
  match config.language = Flow with
  | true -> typeReactElementFlow
  | false -> typeReactChildTypeScript

let isTypeReactElement ~config type_ = type_ == typeReactElement ~config

let typeReactDOMReDomRef ~config =
  (match config.language = Flow with
  | true -> "React$Ref"
  | false -> "React.Ref")
  |> ident ~builtin:true ~typeArgs:[mixedOrUnknown ~config]

let typeReactEventMouseT = "MouseEvent" |> ident ~builtin:true

let reactRefCurrent = "current"

let typeReactRef ~type_ =
  Object
    ( Open,
      [
        {
          mutable_ = Mutable;
          nameJS = reactRefCurrent;
          nameRE = reactRefCurrent;
          optional = Mandatory;
          type_ = Null type_;
        };
      ] )

let isTypeReactRef ~fields =
  match fields with
  | [{mutable_ = Mutable; nameJS; nameRE; optional = Mandatory}] ->
    nameJS == reactRefCurrent && nameJS == nameRE
  | _ -> false

let isTypeFunctionComponent ~config ~fields type_ =
  type_ |> isTypeReactElement ~config && not (isTypeReactRef ~fields)

let componentExportName ~config ~fileName ~moduleName =
  match config.language with
  | Flow -> (
    match fileName = moduleName with
    | true -> "component"
    | false -> moduleName |> ModuleName.toString)
  | _ -> moduleName |> ModuleName.toString

let typeAny ~config =
  ident ~builtin:true
    (match config.language = Flow with
    | true ->
      config.emitFlowAny <- true;
      "$any"
    | false -> "any")

let rec renderType ~config ?(indent = None) ~typeNameIsInterface ~inFunType
    type0 =
  match type0 with
  | Array (t, arrayKind) ->
    let typeIsSimple =
      match t with Ident _ | TypeVar _ -> true | _ -> false
    in
    if config.language = TypeScript && typeIsSimple && arrayKind = Mutable then
      (t |> renderType ~config ~indent ~typeNameIsInterface ~inFunType) ^ "[]"
    else
      let arrayName =
        match arrayKind = Mutable with
        | true -> "Array"
        | false -> (
          match config.language = Flow with
          | true -> "$ReadOnlyArray"
          | false -> "ReadonlyArray")
      in
      arrayName ^ "<"
      ^ (t |> renderType ~config ~indent ~typeNameIsInterface ~inFunType)
      ^ ">"
  | Function
      {argTypes = [{aType = Object (closedFlag, fields)}]; retType; typeVars}
    when retType |> isTypeFunctionComponent ~config ~fields ->
    let fields =
      fields
      |> List.map (fun field ->
             {
               field with
               type_ =
                 field.type_
                 |> TypeVars.substitute ~f:(fun s ->
                        if typeVars |> List.mem s then Some (typeAny ~config)
                        else None);
             })
    in
    let componentType =
      typeReactComponent ~config ~propsType:(Object (closedFlag, fields))
    in
    componentType |> renderType ~config ~indent ~typeNameIsInterface ~inFunType
  | Function {argTypes; retType; typeVars} ->
    renderFunType ~config ~indent ~inFunType ~typeNameIsInterface ~typeVars
      argTypes retType
  | GroupOfLabeledArgs fields | Object (_, fields) | Record fields ->
    let indent1 = fields |> Indent.heuristicFields ~indent in
    let config =
      match type0 with
      | GroupOfLabeledArgs _ -> {config with exportInterfaces = false}
      | _ -> config
    in
    let closedFlag =
      match type0 with Object (closedFlag, _) -> closedFlag | _ -> Closed
    in
    fields
    |> renderFields ~closedFlag ~config ~indent:indent1 ~inFunType
         ~typeNameIsInterface
  | Ident {builtin; name; typeArgs} ->
    let name = name |> sanitizeTypeName in
    (match
       (not builtin) && config.exportInterfaces && name |> typeNameIsInterface
     with
    | true -> name |> interfaceName ~config
    | false -> name)
    ^ EmitText.genericsString
        ~typeVars:
          (typeArgs
          |> List.map
               (renderType ~config ~indent ~typeNameIsInterface ~inFunType))
  | Null type_ ->
    "(null | "
    ^ (type_ |> renderType ~config ~indent ~typeNameIsInterface ~inFunType)
    ^ ")"
  | Nullable type_ | Option type_ -> (
    let useParens x =
      match type_ with Function _ | Variant _ -> EmitText.parens [x] | _ -> x
    in
    match config.language with
    | Flow | Untyped ->
      "?"
      ^ useParens
          (type_ |> renderType ~config ~indent ~typeNameIsInterface ~inFunType)
    | TypeScript ->
      "(null | undefined | "
      ^ useParens
          (type_ |> renderType ~config ~indent ~typeNameIsInterface ~inFunType)
      ^ ")")
  | Promise type_ ->
    "Promise" ^ "<"
    ^ (type_ |> renderType ~config ~indent ~typeNameIsInterface ~inFunType)
    ^ ">"
  | Tuple innerTypes ->
    "["
    ^ (innerTypes
      |> List.map (renderType ~config ~indent ~typeNameIsInterface ~inFunType)
      |> String.concat ", ")
    ^ "]"
  | TypeVar s -> s
  | Variant {noPayloads; payloads; polymorphic; unboxed} ->
    let noPayloadsRendered = noPayloads |> List.map labelJSToString in
    let field ~name value =
      {
        mutable_ = Mutable;
        nameJS = name;
        nameRE = name;
        optional = Mandatory;
        type_ = TypeVar value;
      }
    in
    let fields fields =
      fields
      |> renderFields ~closedFlag:Closed ~config ~indent ~inFunType
           ~typeNameIsInterface
    in
    let payloadsRendered =
      payloads
      |> List.map (fun {case; t = type_} ->
             let typeRendered =
               type_
               |> renderType ~config ~indent ~typeNameIsInterface ~inFunType
             in
             match unboxed with
             | true -> typeRendered
             | false ->
               [
                 case |> labelJSToString
                 |> field ~name:(Runtime.jsVariantTag ~polymorphic);
                 typeRendered
                 |> field ~name:(Runtime.jsVariantValue ~polymorphic);
               ]
               |> fields)
    in
    let rendered = noPayloadsRendered @ payloadsRendered in
    let indent1 = rendered |> Indent.heuristicVariants ~indent in
    (match indent1 = None with
    | true -> ""
    | false -> Indent.break ~indent:indent1 ^ "  ")
    ^ (rendered
      |> String.concat
           ((match indent1 = None with
            | true -> " "
            | false -> Indent.break ~indent:indent1)
           ^ "| "))

and renderField ~config ~indent ~typeNameIsInterface ~inFunType
    {mutable_; nameJS = lbl; optional; type_} =
  let optMarker = match optional == Optional with true -> "?" | false -> "" in
  let mutMarker =
    match mutable_ = Immutable with
    | true -> (
      match config.language = Flow with true -> "+" | false -> "readonly ")
    | false -> ""
  in
  Indent.break ~indent ^ mutMarker ^ lbl ^ optMarker ^ ": "
  ^ (type_ |> renderType ~config ~indent ~typeNameIsInterface ~inFunType)

and renderFields ~closedFlag ~config ~indent ~inFunType ~typeNameIsInterface
    fields =
  let indent1 = indent |> Indent.more in
  let exact =
    config.language = Flow
    && (not config.exportInterfaces)
    && closedFlag = Closed
  in
  let space =
    match indent = None && fields <> [] with true -> " " | false -> ""
  in
  let renderedFields =
    fields
    |> List.map
         (renderField ~config ~indent:indent1 ~typeNameIsInterface ~inFunType)
  in
  let dotdotdot =
    match config.language = Flow && not exact with
    | true -> [Indent.break ~indent:indent1 ^ "..."]
    | false -> []
  in
  ((match exact with true -> "{|" | false -> "{") ^ space)
  ^ String.concat
      (match config.language = TypeScript with true -> "; " | false -> ", ")
      (renderedFields @ dotdotdot)
  ^ Indent.break ~indent ^ space
  ^ match exact with true -> "|}" | false -> "}"

and renderFunType ~config ~indent ~inFunType ~typeNameIsInterface ~typeVars
    argTypes retType =
  (match inFunType with true -> "(" | false -> "")
  ^ EmitText.genericsString ~typeVars
  ^ "("
  ^ String.concat ", "
      (List.mapi
         (fun i {aName; aType} ->
           let parameterName =
             if config.language = Flow then ""
             else
               (match aName = "" with
               | true -> "_" ^ string_of_int (i + 1)
               | false -> aName)
               ^ ":"
           in
           parameterName
           ^ (aType
             |> renderType ~config ~indent ~typeNameIsInterface ~inFunType:true
             ))
         argTypes)
  ^ ") => "
  ^ (retType |> renderType ~config ~indent ~typeNameIsInterface ~inFunType)
  ^ match inFunType with true -> ")" | false -> ""

let typeToString ~config ~typeNameIsInterface type_ =
  type_ |> renderType ~config ~typeNameIsInterface ~inFunType:false

let ofType ~config ?(typeNameIsInterface = fun _ -> false) ~type_ s =
  match config.language = Untyped with
  | true -> s
  | false -> s ^ ": " ^ (type_ |> typeToString ~config ~typeNameIsInterface)

let emitHookTypeAsFunction ~config ~emitters ~name ~propsType ~retType ~retValue
    ~typeNameIsInterface ~typeVars =
  "// Type annotated function components are not checked by Flow, but typeof() \
   works.\n" ^ "const " ^ name ^ " = function "
  ^ EmitText.genericsString ~typeVars
  ^ "("
  ^ ("_: "
    ^ (propsType |> renderType ~config ~typeNameIsInterface ~inFunType:true))
  ^ ")"
  ^ (" " |> ofType ~config ~typeNameIsInterface ~type_:retType)
  ^ " { return " ^ retValue ^ " };"
  |> Emitters.export ~emitters

let emitExportConst_ ~early ?(comment = "") ~config ?(docString = "") ~emitters
    ~name ~type_ ~typeNameIsInterface line =
  ((match comment = "" with true -> comment | false -> "// " ^ comment ^ "\n")
  ^ docString
  ^
  match (config.module_, config.language) with
  | _, TypeScript | ES6, _ ->
    "export const "
    ^ (name |> ofType ~config ~typeNameIsInterface ~type_)
    ^ " = " ^ line
  | CommonJS, _ ->
    "const "
    ^ (name |> ofType ~config ~typeNameIsInterface ~type_)
    ^ " = " ^ line ^ ";\nexports." ^ name ^ " = " ^ name)
  |> (match early with
     | true -> Emitters.exportEarly
     | false -> Emitters.export)
       ~emitters

let emitExportConst = emitExportConst_ ~early:false

let emitExportConstEarly = emitExportConst_ ~early:true

let emitExportFunction ~early ~comment ~emitters ~name ~config line =
  (("// " ^ comment ^ "\n")
  ^
  match (config.module_, config.language) with
  | _, TypeScript | ES6, _ -> "export function " ^ name ^ line
  | CommonJS, _ ->
    "function " ^ name ^ line ^ ";\nexports." ^ name ^ " = " ^ name)
  |> (match early with
     | true -> Emitters.exportEarly
     | false -> Emitters.export)
       ~emitters

let emitExportDefault ~emitters ~config name =
  match (config.module_, config.language) with
  | _, TypeScript | ES6, _ ->
    "export default " ^ name ^ ";" |> Emitters.export ~emitters
  | CommonJS, _ ->
    "exports.default = " ^ name ^ ";" |> Emitters.export ~emitters

let emitExportType ?(early = false) ~config ~emitters ~nameAs ~opaque ~type_
    ~typeNameIsInterface ~typeVars resolvedTypeName =
  let export =
    match early with true -> Emitters.exportEarly | false -> Emitters.export
  in
  let typeParamsString = EmitText.genericsString ~typeVars in
  let isInterface = resolvedTypeName |> typeNameIsInterface in
  let resolvedTypeName =
    match config.exportInterfaces && isInterface with
    | true -> resolvedTypeName |> interfaceName ~config
    | false -> resolvedTypeName
  in
  let exportNameAs =
    match nameAs with
    | None -> ""
    | Some s ->
      "\nexport type " ^ s ^ typeParamsString ^ " = " ^ resolvedTypeName
      ^ typeParamsString ^ ";"
  in
  match config.language with
  | Flow ->
    if config.exportInterfaces && isInterface && not opaque then
      "export interface " ^ resolvedTypeName ^ typeParamsString ^ " "
      ^ ((match opaque with true -> mixedOrUnknown ~config | false -> type_)
        |> typeToString ~config ~typeNameIsInterface)
      ^ ";" ^ exportNameAs
      |> export ~emitters
    else
      "export"
      ^ (match opaque with true -> " opaque " | false -> " ")
      ^ "type " ^ resolvedTypeName ^ typeParamsString ^ " = "
      ^ ((match opaque with true -> mixedOrUnknown ~config | false -> type_)
        |> typeToString ~config ~typeNameIsInterface)
      ^ ";" ^ exportNameAs
      |> export ~emitters
  | TypeScript ->
    if opaque then
      (* Represent an opaque type as an absract class with a field called 'opaque'.
         Any type parameters must occur in the type of opaque, so that different
         instantiations are considered different types. *)
      let typeOfOpaqueField =
        match typeVars = [] with
        | true -> "any"
        | false -> typeVars |> String.concat " | "
      in
      "// tslint:disable-next-line:max-classes-per-file \n"
      ^ (match String.capitalize_ascii resolvedTypeName <> resolvedTypeName with
        | true -> "// tslint:disable-next-line:class-name\n"
        | false -> "")
      ^ "export abstract class " ^ resolvedTypeName ^ typeParamsString
      ^ " { protected opaque!: " ^ typeOfOpaqueField
      ^ " }; /* simulate opaque types */" ^ exportNameAs
      |> export ~emitters
    else
      (if isInterface && config.exportInterfaces then
       "export interface " ^ resolvedTypeName ^ typeParamsString ^ " "
      else
        "// tslint:disable-next-line:interface-over-type-literal\n"
        ^ "export type " ^ resolvedTypeName ^ typeParamsString ^ " = ")
      ^ (match type_ with
        | _ -> type_ |> typeToString ~config ~typeNameIsInterface)
      ^ ";" ^ exportNameAs
      |> export ~emitters
  | Untyped -> emitters

let emitImportValueAsEarly ~config ~emitters ~name ~nameAs importPath =
  let commentBeforeImport =
    match config.language = Flow with
    | true -> "// flowlint-next-line nonstrict-import:off\n"
    | false -> ""
  in
  commentBeforeImport ^ "import "
  ^ (match nameAs with
    | Some nameAs -> "{" ^ name ^ " as " ^ nameAs ^ "}"
    | None -> name)
  ^ " from " ^ "'"
  ^ (importPath |> ImportPath.emit ~config)
  ^ "';"
  |> Emitters.requireEarly ~emitters

let emitRequire ~importedValueOrComponent ~early ~emitters ~config ~moduleName
    ~strict importPath =
  let commentBeforeRequire =
    match config.language with
    | TypeScript -> (
      match importedValueOrComponent with
      | true -> "// tslint:disable-next-line:no-var-requires\n"
      | false -> "// @ts-ignore: Implicit any on import\n")
    | Flow -> (
      match strict with
      | true -> (
        match early with
        | true -> "// flowlint-next-line nonstrict-import:off\n"
        | false -> "")
      | false -> flowExpectedError)
    | Untyped -> ""
  in
  match config.module_ with
  | ES6 when not importedValueOrComponent ->
    let moduleNameString = ModuleName.toString moduleName in
    (if config.language = TypeScript then
     let es6ImportModule = moduleNameString ^ "__Es6Import" in
     commentBeforeRequire ^ "import * as " ^ es6ImportModule ^ " from '"
     ^ (importPath |> ImportPath.emit ~config)
     ^ "';\n" ^ "const " ^ moduleNameString ^ ": any = " ^ es6ImportModule ^ ";"
    else
      commentBeforeRequire ^ "import * as " ^ moduleNameString ^ " from '"
      ^ (importPath |> ImportPath.emit ~config)
      ^ "';")
    |> (match early with
       | true -> Emitters.requireEarly
       | false -> Emitters.require)
         ~emitters
  | _ ->
    commentBeforeRequire ^ "const "
    ^ ModuleName.toString moduleName
    ^ " = require('"
    ^ (importPath |> ImportPath.emit ~config)
    ^ "');"
    |> (match early with
       | true -> Emitters.requireEarly
       | false -> Emitters.require)
         ~emitters

let require ~early =
  match early with true -> Emitters.requireEarly | false -> Emitters.require

let emitImportReact ~emitters ~config =
  match config.language with
  | Flow | Untyped ->
    emitRequire ~importedValueOrComponent:false ~early:true ~emitters ~config
      ~moduleName:ModuleName.react ~strict:false ImportPath.react
  | TypeScript ->
    "import * as React from 'react';" |> require ~early:true ~emitters

let emitPropTypes ~config ~emitters ~indent ~name fields =
  let indent1 = indent |> Indent.more in
  let prefix s = "PropTypes." ^ s in
  let rec emitType ~indent (type_ : type_) =
    match type_ with
    | Array (t, _) -> prefix "arrayOf" ^ "(" ^ (t |> emitType ~indent) ^ ")"
    | Ident {name = ("bool" | "number" | "string") as id} -> id |> prefix
    | Function _ -> "func" |> prefix
    | GroupOfLabeledArgs fields | Object (_, fields) | Record fields ->
      let indent1 = indent |> Indent.more in
      prefix "shape" ^ "({"
      ^ Indent.break ~indent:indent1
      ^ (fields
        |> List.filter (fun ({nameJS} : field) -> nameJS <> "children")
        |> List.map (emitField ~indent:indent1)
        |> String.concat ("," ^ Indent.break ~indent:indent1))
      ^ Indent.break ~indent ^ "})"
    | Ident _ | Null _ | Nullable _ | Option _ | Promise _ | Tuple _ | TypeVar _
    | Variant _ ->
      "any" |> prefix
  and emitField ~indent ({nameJS; optional; type_} : field) =
    nameJS ^ " : "
    ^ (type_ |> emitType ~indent)
    ^ match optional = Mandatory with true -> ".isRequired" | false -> ""
  in
  config.emitImportPropTypes <- true;
  name ^ ".propTypes = " ^ "{"
  ^ Indent.break ~indent:indent1
  ^ (fields
    |> List.filter (fun ({nameJS} : field) -> nameJS <> "children")
    |> List.map (emitField ~indent:indent1)
    |> String.concat ("," ^ Indent.break ~indent:indent1))
  ^ Indent.break ~indent ^ "};"
  |> Emitters.export ~emitters

let emitImportTypeAs ~emitters ~config ~typeName ~asTypeName
    ~typeNameIsInterface ~importPath =
  let typeName = sanitizeTypeName typeName in
  let asTypeName =
    match asTypeName with
    | None -> asTypeName
    | Some s -> Some (sanitizeTypeName s)
  in
  let typeName, asTypeName =
    match asTypeName with
    | Some asName -> (
      match asName |> typeNameIsInterface with
      | true ->
        ( typeName |> interfaceName ~config,
          Some (asName |> interfaceName ~config) )
      | false -> (typeName, asTypeName))
    | None -> (typeName, asTypeName)
  in
  let importPathString = importPath |> ImportPath.emit ~config in
  let strictLocalPrefix =
    match
      (not
         (Filename.check_suffix importPathString
            (generatedFilesExtension ~config)))
      && config.language = Flow
    with
    | true -> "// flowlint-next-line nonstrict-import:off\n"
    | false -> ""
  in
  match config.language with
  | Flow | TypeScript ->
    strictLocalPrefix ^ "import "
    ^ (match config.language = Flow with true -> "type " | false -> "")
    ^ "{" ^ typeName
    ^ (match asTypeName with Some asT -> " as " ^ asT | None -> "")
    ^ "} from '" ^ importPathString ^ "';"
    |> Emitters.import ~emitters
  | Untyped -> emitters

let ofTypeAny ~config s = s |> ofType ~config ~type_:(typeAny ~config)

let emitTypeCast ~config ~type_ ~typeNameIsInterface s =
  match config.language with
  | TypeScript ->
    s ^ " as " ^ (type_ |> typeToString ~config ~typeNameIsInterface)
  | Untyped | Flow -> s
