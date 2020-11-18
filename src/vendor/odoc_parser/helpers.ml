(* This file contains mostly functions from the former [model/attrs.ml]. It
   should be reorganized in the future. *)

module Paths = Paths

(* This should be merged into [Parse_error] above. *)
exception InvalidReference of string

let read_qualifier :
  string option ->
  [< Paths.Reference.kind ] Paths.Reference.tag
  = function
  | None -> TUnknown
  | Some "module" -> TModule
  | Some "module-type" -> TModuleType
  | Some "type" -> TType
  | Some ("const" | "constructor") -> TConstructor
  | Some ("recfield" | "field") -> TField
  | Some "extension" -> TExtension
  | Some ("exn" | "exception") -> TException
  | Some ("val" | "value") -> TValue
  | Some "class" -> TClass
  | Some ("classtype" | "class-type") -> TClassType
  | Some "method" -> TMethod
  | Some "instance-variable" -> TInstanceVariable
  | Some ("section" | "label") -> TLabel
  | Some ("page") -> TPage
  | Some s -> raise (InvalidReference ("unknown qualifier `" ^ s ^ "'"))

let read_longident s =
  let open Paths.Reference in
  let split_qualifier str =
    match String.rindex str '-' with
    | exception Not_found -> (None, str)
    | idx ->
      let qualifier = String.sub str 0 idx in
      let name = String.sub str (idx + 1) (String.length str - idx - 1) in
      (Some qualifier, name)
  in
  let rec loop_datatype : string -> int -> datatype option =
    fun s pos ->
      match String.rindex_from s pos '.' with
      | exception Not_found ->
        let maybe_qualified = String.sub s 0 (pos + 1) in
        if String.length maybe_qualified = 0 then
          None
        else
          let (kind, name) = split_qualifier maybe_qualified in
          begin match read_qualifier kind with
          | TUnknown | TType as tag -> Some (Root(name, tag))
          | _ -> None
          end
      | idx ->
        let maybe_qualified = String.sub s (idx + 1) (pos - idx) in
        if String.length maybe_qualified = 0 then
          None
        else
          let (qualifier, name) = split_qualifier maybe_qualified in
          match read_qualifier qualifier with
          | TUnknown -> begin
              match loop_parent s (idx - 1) with
              | None -> None
              | Some parent -> Some (Dot(label_parent_of_parent parent, name))
            end
          | TType -> begin
              match loop_signature s (idx - 1) with
              | None -> None
              | Some parent -> Some (Type(parent, name))
            end
          | _ -> None
  and loop_signature : string -> int -> signature option = fun s pos ->
      match String.rindex_from s pos '.' with
      | exception Not_found ->
        let maybe_qualified = String.sub s 0 (pos + 1) in
        if String.length maybe_qualified = 0 then
          None
        else
          let (kind, name) = split_qualifier maybe_qualified in
          begin match read_qualifier kind with
          | TUnknown | TModule | TModuleType as tag -> Some (Root(name, tag))
          | _ -> None
          end
      | idx ->
        let maybe_qualified = String.sub s (idx + 1) (pos - idx) in
        if String.length maybe_qualified = 0 then
          None
        else
          let (qualifier, name) = split_qualifier maybe_qualified in
          match read_qualifier qualifier with
          | TUnknown -> begin
              match loop_parent s (idx - 1) with
              | None -> None
              | Some parent -> Some (Dot(label_parent_of_parent parent, name))
            end
          | TModule -> begin
              match loop_signature s (idx - 1) with
              | None -> None
              | Some parent -> Some (Module(parent, name))
            end
          | TModuleType -> begin
              match loop_signature s (idx - 1) with
              | None -> None
              | Some parent -> Some (ModuleType(parent, name))
            end
          | _ -> None
  and loop_class_signature : string -> int -> class_signature option =
    fun s pos ->
      match String.rindex_from s pos '.' with
      | exception Not_found ->
        let maybe_qualified = String.sub s 0 (pos + 1) in
        if String.length maybe_qualified = 0 then
          None
        else
          let (kind, name) = split_qualifier maybe_qualified in
          begin match read_qualifier kind with
          | TUnknown | TClass | TClassType as tag -> Some (Root(name, tag))
          | _ -> None
          end
      | idx ->
        let maybe_qualified = String.sub s (idx + 1) (pos - idx) in
        if String.length maybe_qualified = 0 then
          None
        else
          let (qualifier, name) = split_qualifier maybe_qualified in
          match read_qualifier qualifier with
          | TUnknown -> begin
              match loop_parent s (idx - 1) with
              | None -> None
              | Some parent -> Some (Dot(label_parent_of_parent parent, name))
            end
          | TClass -> begin
              match loop_signature s (idx - 1) with
              | None -> None
              | Some parent -> Some (Class(parent, name))
            end
          | TClassType -> begin
              match loop_signature s (idx - 1) with
              | None -> None
              | Some parent -> Some (ClassType(parent, name))
            end
          | _ -> None
  and loop_label_parent : string -> int -> label_parent option =
    fun s pos ->
      match String.rindex_from s pos '.' with
      | exception Not_found ->
        let maybe_qualified = String.sub s 0 (pos + 1) in
        if String.length maybe_qualified = 0 then
          None
        else
          let (kind, name) = split_qualifier maybe_qualified in
          begin match read_qualifier kind with
          | TUnknown | TModule | TModuleType
          | TType | TClass | TClassType | TPage as tag ->
            Some (Root(name, tag))
          | _ -> None
          end
      | idx ->
        let maybe_qualified = String.sub s (idx + 1) (pos - idx) in
        if String.length maybe_qualified = 0 then
          None
        else
          let (qualifier, name) = split_qualifier maybe_qualified in
          match read_qualifier qualifier with
          | TUnknown -> begin
              match loop_label_parent s (idx - 1) with
              | None -> None
              | Some parent -> Some (Dot(parent, name))
            end
          | TModule -> begin
              match loop_signature s (idx - 1) with
              | None -> None
              | Some parent -> Some (Module(parent, name))
            end
          | TModuleType -> begin
              match loop_signature s (idx - 1) with
              | None -> None
              | Some parent -> Some (ModuleType(parent, name))
            end
          | TType -> begin
              match loop_signature s (idx - 1) with
              | None -> None
              | Some parent -> Some (Type(parent, name))
            end
          | TClass -> begin
              match loop_signature s (idx - 1) with
              | None -> None
              | Some parent -> Some (Class(parent, name))
            end
          | TClassType -> begin
              match loop_signature s (idx - 1) with
              | None -> None
              | Some parent -> Some (ClassType(parent, name))
            end
          | _ -> None
  and loop_parent : string -> int -> parent option =
    fun s pos ->
      match String.rindex_from s pos '.' with
      | exception Not_found ->
        let maybe_qualified = String.sub s 0 (pos + 1) in
        if String.length maybe_qualified = 0 then
          None
        else
          let (kind, name) = split_qualifier maybe_qualified in
          begin match read_qualifier kind with
          | TUnknown
          | TModule | TModuleType | TType | TClass | TClassType as tag ->
            Some (Root(name, tag))
          | _ -> None
          end
      | idx ->
        let maybe_qualified = String.sub s (idx + 1) (pos - idx) in
        if String.length maybe_qualified = 0 then
          None
        else
          let (qualifier, name) = split_qualifier maybe_qualified in
          match read_qualifier qualifier with
          | TUnknown -> begin
              match loop_parent s (idx - 1) with
              | None -> None
              | Some parent -> Some (Dot(label_parent_of_parent parent, name))
            end
          | TModule -> begin
              match loop_signature s (idx - 1) with
              | None -> None
              | Some parent -> Some (Module(parent, name))
            end
          | TModuleType -> begin
              match loop_signature s (idx - 1) with
              | None -> None
              | Some parent -> Some (ModuleType(parent, name))
            end
          | TType -> begin
              match loop_signature s (idx - 1) with
              | None -> None
              | Some parent -> Some (Type(parent, name))
            end
          | TClass -> begin
              match loop_signature s (idx - 1) with
              | None -> None
              | Some parent -> Some (Class(parent, name))
            end
          | TClassType -> begin
              match loop_signature s (idx - 1) with
              | None -> None
              | Some parent -> Some (ClassType(parent, name))
            end
          | _ -> None
  in
  let loop : 'k. string -> int -> kind t option =
    fun s pos ->
      match String.rindex_from s pos '.' with
      | exception Not_found ->
        let maybe_qualified = String.sub s 0 (pos + 1) in
        if String.length maybe_qualified = 0 then
          None
        else
          let (kind, name) = split_qualifier maybe_qualified in
          Some (Root (name, read_qualifier kind))
      | idx ->
        let maybe_qualified = String.sub s (idx + 1) (pos - idx) in
        if String.length maybe_qualified = 0 then
          None
        else
          let (qualifier, name) = split_qualifier maybe_qualified in
          match read_qualifier qualifier with
          | TUnknown -> begin
              match loop_label_parent s (idx - 1) with
              | None -> None
              | Some parent -> Some (Dot(parent, name))
            end
          | TModule -> begin
              match loop_signature s (idx - 1) with
              | None -> None
              | Some parent -> Some (Module(parent, name))
            end
          | TModuleType -> begin
              match loop_signature s (idx - 1) with
              | None -> None
              | Some parent -> Some (ModuleType(parent, name))
            end
          | TType -> begin
              match loop_signature s (idx - 1) with
              | None -> None
              | Some parent -> Some (Type(parent, name))
            end
          | TConstructor -> begin
              match loop_datatype s (idx - 1) with
              | None -> None
              | Some parent -> Some (Constructor(parent, name))
            end
          | TField -> begin
              match loop_parent s (idx - 1) with
              | None -> None
              | Some parent -> Some (Field(parent, name))
            end
          | TExtension -> begin
              match loop_signature s (idx - 1) with
              | None -> None
              | Some parent -> Some (Extension(parent, name))
            end
          | TException -> begin
              match loop_signature s (idx - 1) with
              | None -> None
              | Some parent -> Some (Exception(parent, name))
            end
          | TValue -> begin
              match loop_signature s (idx - 1) with
              | None -> None
              | Some parent -> Some (Value(parent, name))
            end
          | TClass -> begin
              match loop_signature s (idx - 1) with
              | None -> None
              | Some parent -> Some (Class(parent, name))
            end
          | TClassType -> begin
              match loop_signature s (idx - 1) with
              | None -> None
              | Some parent -> Some (ClassType(parent, name))
            end
          | TMethod -> begin
              match loop_class_signature s (idx - 1) with
              | None -> None
              | Some parent -> Some (Method(parent, name))
            end
          | TInstanceVariable -> begin
              match loop_class_signature s (idx - 1) with
              | None -> None
              | Some parent -> Some (InstanceVariable(parent, name))
            end
          | TLabel -> begin
              match loop_label_parent s (idx - 1) with
              | None -> None
              | Some parent -> Some (Label(parent, name))
            end
          | TPage -> None
  in
    match loop s (String.length s - 1) with
    | None -> raise (InvalidReference s)
    | Some r -> r

let read_reference (s : string) : Paths.Reference.any =
  let s =
    match String.rindex s ':' with
    | index -> String.sub s (index + 1) (String.length s - (index + 1))
    | exception Not_found -> s
  in
  read_longident s

let read_path_longident s =
  let open Paths.Path in
  let rec loop : 'k. string -> int -> ([< kind > `Module ] as 'k) t option =
    fun s pos ->
      try
        let idx = String.rindex_from s pos '.' in
        let name = String.sub s (idx + 1) (pos - idx) in
        if String.length name = 0 then None
        else
          match loop s (idx - 1) with
          | None -> None
          | Some parent -> Some (Dot(parent, name))
      with Not_found ->
        let name = String.sub s 0 (pos + 1) in
        if String.length name = 0 then None
        else Some (Root name)
  in
    match loop s (String.length s - 1) with
    | None -> raise (InvalidReference s)
    | Some r -> r

exception Expected_reference_to_a_module_but_got of string

let read_mod_longident lid : Paths.Reference.module_ =
  let open Paths.Reference in
  match read_longident lid with
  | Root (_, (TUnknown | TModule))
  | Dot (_, _)
  | Module (_,_) as r -> r
  | _ ->
      (* FIXME: propagate location *)
      raise (Expected_reference_to_a_module_but_got lid)
