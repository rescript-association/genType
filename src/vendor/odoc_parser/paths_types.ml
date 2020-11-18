(** {1 Paths} *)

(** Every path is annotated with its kind. *)
module Kind =
struct
  (** {4 General purpose kinds} *)

  (** Any possible referent *)
  type any =
    [ `Module | `ModuleType | `Type
    | `Constructor | `Field | `Extension
    | `Exception | `Value | `Class | `ClassType
    | `Method | `InstanceVariable | `Label | `Page ]

  (** A referent that can contain signature items *)
  type signature = [ `Module | `ModuleType ]

  (** A referent that can contain class signature items *)
  type class_signature = [ `Class | `ClassType ]

  (** A referent that can contain datatype items *)
  type datatype = [ `Type ]

  (** A referent that can contain page items *)
  type page = [ `Page ]

  (** A referent that can contain other items *)
  type parent = [ signature | class_signature | datatype ]

  type label_parent = [ parent | page ]

  (** {4 Identifier kinds}

      The kind of an identifier directly corresponds to the kind of its
      referent. *)

  type identifier = any

  type identifier_module = [ `Module ]
  type identifier_module_type = [ `ModuleType ]
  type identifier_type =  [ `Type ]
  type identifier_constructor = [ `Constructor ]
  type identifier_field = [ `Field ]
  type identifier_extension = [ `Extension ]
  type identifier_exception = [ `Exception ]
  type identifier_value = [ `Value ]
  type identifier_class = [ `Class ]
  type identifier_class_type = [ `ClassType ]
  type identifier_method = [ `Method ]
  type identifier_instance_variable = [ `InstanceVariable ]
  type identifier_label = [ `Label ]
  type identifier_page = [ `Page ]

  (** {4 Path kinds}

      There are four kinds of OCaml path:

        - module
        - module type
        - type
        - class type

      These kinds do not directly correspond to the kind of their
      referent (e.g. a type path may refer to a class definition). *)

  type path = [ `Module | `ModuleType | `Type | `Class | `ClassType ]

  type path_module = [ `Module ]
  type path_module_type = [ `ModuleType ]
  type path_type = [ `Type | `Class | `ClassType ]
  type path_class_type = [ `Class | `ClassType ]

  (** {4 Fragment kinds}

      There are two kinds of OCaml path fragment:

        - module
        - type

      These kinds do not directly correspond to the kind of their
      referent (e.g. a type path fragment may refer to a class
      definition). *)

  type fragment = [ `Module | `Type | `Class | `ClassType ]

  type fragment_module = [ `Module ]
  type fragment_type = [ `Type | `Class | `ClassType ]

  (** {4 Reference kinds}

      There is one reference kind for each kind of referent. However,
      the kind of a reference does not refer to the kind of its
      referent, but to the kind with which the reference was annotated.

      This means that reference kinds do not correspond directly to the
      kind of their referent because we used more relaxed rules when
      resolving a reference. For example, a reference annotated as being
      to a constructor can be resolved to the definition of an exception
      (which is a sort of constructor). *)

  type reference = any

  type reference_module = [ `Module ]
  type reference_module_type = [ `ModuleType ]
  type reference_type = [ `Type | `Class | `ClassType ]
  type reference_constructor = [ `Constructor | `Extension | `Exception ]
  type reference_field = [ `Field ]
  type reference_extension = [ `Extension | `Exception ]
  type reference_exception = [ `Exception ]
  type reference_value = [ `Value ]
  type reference_class = [ `Class ]
  type reference_class_type = [ `Class | `ClassType ]
  type reference_method = [ `Method ]
  type reference_instance_variable = [ `InstanceVariable ]
  type reference_label = [ `Label ]
  type reference_page = [ `Page ]
end

module Identifier =
struct
  type kind = Kind.identifier

  type 'kind t =
    | Root : Root.t * string -> [< kind > `Module] t
    | Page : Root.t * string -> [< kind > `Page] t
    | Module : signature * string -> [< kind > `Module] t
    | Argument : signature * int * string -> [< kind > `Module] t
    | ModuleType : signature * string -> [< kind > `ModuleType] t
    | Type : signature * string -> [< kind > `Type] t
    | CoreType : string -> [< kind > `Type] t
    | Constructor : datatype * string -> [< kind > `Constructor] t
    | Field : parent * string -> [< kind > `Field] t
    | Extension : signature * string -> [< kind > `Extension] t
    | Exception : signature * string -> [< kind > `Exception] t
    | CoreException : string -> [< kind > `Exception] t
    | Value : signature * string -> [< kind > `Value] t
    | Class : signature * string -> [< kind > `Class] t
    | ClassType : signature * string -> [< kind > `ClassType] t
    | Method : class_signature * string -> [< kind > `Method] t
    | InstanceVariable : class_signature * string ->
        [< kind > `InstanceVariable] t
    | Label : label_parent * string -> [< kind > `Label] t

  and any = kind t
  and signature = Kind.signature t
  and class_signature = Kind.class_signature t
  and datatype = Kind.datatype t
  and parent = Kind.parent t
  and label_parent = Kind.label_parent t

  type module_ = Kind.identifier_module t
  type module_type = Kind.identifier_module_type t
  type type_ = Kind.identifier_type t
  type constructor = Kind.identifier_constructor t
  type field = Kind.identifier_field t
  type extension = Kind.identifier_extension t
  type exception_ = Kind.identifier_exception t
  type value = Kind.identifier_value t
  type class_ = Kind.identifier_class t
  type class_type = Kind.identifier_class_type t
  type method_ = Kind.identifier_method t
  type instance_variable = Kind.identifier_instance_variable t
  type label = Kind.identifier_label t
  type page = Kind.identifier_page t

  type path_module = Kind.path_module t
  type path_module_type = Kind.path_module_type t
  type path_type = Kind.path_type t
  type path_class_type = Kind.path_class_type t

  type fragment_module = Kind.fragment_module t
  type fragment_type = Kind.fragment_type t

  type reference_module = Kind.reference_module t
  type reference_module_type = Kind.reference_module_type t
  type reference_type =  Kind.reference_type t
  type reference_constructor = Kind.reference_constructor t
  type reference_field = Kind.reference_field t
  type reference_extension = Kind.reference_extension t
  type reference_exception = Kind.reference_exception t
  type reference_value = Kind.reference_value t
  type reference_class = Kind.reference_class t
  type reference_class_type = Kind.reference_class_type t
  type reference_method = Kind.reference_method t
  type reference_instance_variable = Kind.reference_instance_variable t
  type reference_label = Kind.reference_label t
  type reference_page = Kind.reference_page t
end

module rec Path :
sig
  type kind = Kind.path

  type 'kind t =
    | Resolved : 'kind Resolved_path.t -> 'kind t
    | Root : string -> [< kind > `Module] t
    | Forward : string -> [< kind > `Module] t
    | Dot : module_ * string -> [< kind] t
    | Apply : module_ * module_ -> [< kind > `Module] t

  and any = kind t
  and module_ = Kind.path_module t
  and module_type = Kind.path_module_type t
  and type_ = Kind.path_type t
  and class_type = Kind.path_class_type t
end = Path

and Resolved_path :
sig
  type kind = Kind.path

  type 'kind t =
    | Identifier : 'kind Identifier.t -> ([< kind] as 'kind) t
    | Subst : module_type * module_ -> [< kind > `Module] t
    | SubstAlias : module_ * module_ -> [< kind > `Module] t
    | Hidden : module_ -> [< kind > `Module ] t
    | Module : module_ * string -> [< kind > `Module] t
      (* TODO: The canonical path should be a reference not a path *)
    | Canonical : module_ * Path.module_ -> [< kind > `Module] t
    | Apply : module_ * Path.module_ -> [< kind > `Module] t
    | ModuleType : module_ * string -> [< kind > `ModuleType] t
    | Type : module_ * string -> [< kind > `Type] t
    | Class : module_ * string -> [< kind > `Class] t
    | ClassType : module_ * string -> [< kind > `ClassType] t

  and any = kind t
  and module_ = Kind.path_module t
  and module_type = Kind.path_module_type t
  and type_ = Kind.path_type t
  and class_type = Kind.path_class_type t
end = Resolved_path

module rec Fragment :
sig
  type kind = Kind.fragment

  type sort = [ `Root | `Branch ]

  type ('b, 'c) raw =
    | Resolved : ('b, 'c) Resolved_fragment.raw -> ('b, 'c) raw
    | Dot : signature * string -> ([< kind], [< sort > `Branch]) raw

  and 'b t = ('b, [`Branch]) raw
  and any = kind t
  and signature = (Kind.fragment_module, [`Root | `Branch]) raw

  type module_ = Kind.fragment_module t
  type type_ = Kind.fragment_type t
end = Fragment

and Resolved_fragment :
sig
  type kind = Kind.fragment

  type sort = [ `Root | `Branch ]

  type ('b, 'c) raw =
    | Root : ('b, [< sort > `Root]) raw
    | Subst : Resolved_path.module_type * module_ ->
              ([< kind > `Module] as 'b, [< sort > `Branch] as 'c) raw
    | SubstAlias : Resolved_path.module_ * module_ ->
              ([< kind > `Module] as 'b, [< sort > `Branch] as 'c) raw
    | Module : signature * string -> ([< kind > `Module], [< sort > `Branch]) raw
    | Type : signature * string -> ([< kind > `Type], [< sort > `Branch]) raw
    | Class : signature * string -> ([< kind > `Class], [< sort > `Branch]) raw
    | ClassType : signature * string -> ([< kind > `ClassType], [< sort > `Branch]) raw

  and 'b t = ('b, [`Branch]) raw
  and any = kind t
  and signature = (Kind.fragment_module, [`Root | `Branch]) raw
  and module_ = Kind.fragment_module t

  type type_ = Kind.fragment_type t
end = Resolved_fragment

module rec Reference :
sig
  type kind = Kind.reference

  type _ tag =
    | TUnknown : [< kind ] tag
    | TModule : [< kind > `Module ] tag
    | TModuleType : [< kind > `ModuleType ] tag
    | TType : [< kind > `Type ] tag
    | TConstructor : [< kind > `Constructor ] tag
    | TField : [< kind > `Field ] tag
    | TExtension : [< kind > `Extension ] tag
    | TException : [< kind > `Exception ] tag
    | TValue : [< kind > `Value ] tag
    | TClass : [< kind > `Class ] tag
    | TClassType : [< kind > `ClassType ] tag
    | TMethod : [< kind > `Method ] tag
    | TInstanceVariable : [< kind > `InstanceVariable ] tag
    | TLabel : [< kind > `Label ] tag
    | TPage : [< kind > `Page ] tag

  type 'kind t =
    | Resolved : 'kind Resolved_reference.t -> 'kind t
    | Root : string * 'kind tag -> 'kind t
    | Dot : label_parent * string -> [< kind ] t
    | Module : signature * string -> [< kind > `Module] t
    | ModuleType : signature * string -> [< kind > `ModuleType] t
    | Type : signature * string -> [< kind > `Type] t
    | Constructor : datatype * string -> [< kind > `Constructor] t
    | Field : parent * string -> [< kind > `Field] t
    | Extension : signature * string -> [< kind > `Extension] t
    | Exception : signature * string -> [< kind > `Exception] t
    | Value : signature * string -> [< kind > `Value] t
    | Class : signature * string -> [< kind > `Class] t
    | ClassType : signature * string -> [< kind > `ClassType] t
    | Method : class_signature * string -> [< kind > `Method] t
    | InstanceVariable : class_signature * string ->
        [< kind > `InstanceVariable] t
    | Label : label_parent * string -> [< kind > `Label] t

  and any = kind t
  and signature = Kind.signature t
  and class_signature = Kind.class_signature t
  and datatype = Kind.datatype t
  and parent = Kind.parent t
  and label_parent = [ Kind.parent | Kind.page ] t

  type module_ = Kind.reference_module t
  type module_type = Kind.reference_module_type t
  type type_ = Kind.reference_type t
  type constructor = Kind.reference_constructor t
  type field = Kind.reference_field t
  type extension = Kind.reference_extension t
  type exception_ = Kind.reference_exception t
  type value = Kind.reference_value t
  type class_ = Kind.reference_class t
  type class_type = Kind.reference_class_type t
  type method_ = Kind.reference_method t
  type instance_variable = Kind.reference_instance_variable t
  type label = Kind.reference_label t
  type page = Kind.reference_page t
end = Reference

and Resolved_reference :
sig
  type kind = Kind.reference

  type 'kind t =
    | Identifier : 'kind Identifier.t -> 'kind t
    | SubstAlias : Resolved_path.module_ * module_ -> [< kind > `Module ] t
    | Module : signature * string -> [< kind > `Module] t
    | Canonical : module_ * Reference.module_ -> [< kind > `Module] t
    | ModuleType : signature * string -> [< kind > `ModuleType] t
    | Type : signature * string -> [< kind > `Type] t
    | Constructor : datatype * string -> [< kind > `Constructor] t
    | Field : parent * string -> [< kind > `Field] t
    | Extension : signature * string -> [< kind > `Extension] t
    | Exception : signature * string -> [< kind > `Exception] t
    | Value : signature * string -> [< kind > `Value] t
    | Class : signature * string -> [< kind > `Class] t
    | ClassType : signature * string -> [< kind > `ClassType] t
    | Method : class_signature * string -> [< kind > `Method] t
    | InstanceVariable : class_signature * string ->
        [< kind > `InstanceVariable] t
    | Label : label_parent * string -> [< kind > `Label] t

  and any = kind t
  and signature = Kind.signature t
  and class_signature = Kind.class_signature t
  and datatype = Kind.datatype t
  and parent = Kind.parent t
  and module_ = Kind.reference_module t
  and label_parent = Kind.label_parent t

  type module_type = Kind.reference_module_type t
  type type_ = Kind.reference_type t
  type constructor = Kind.reference_constructor t
  type field = Kind.reference_field t
  type extension = Kind.reference_extension t
  type exception_ = Kind.reference_exception t
  type value = Kind.reference_value t
  type class_ = Kind.reference_class t
  type class_type = Kind.reference_class_type t
  type method_ = Kind.reference_method t
  type instance_variable = Kind.reference_instance_variable t
  type label = Kind.reference_label t
  type page = Kind.reference_page t
end = Resolved_reference
