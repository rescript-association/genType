(*
 * Copyright (c) 2014 Leo White <lpw25@cl.cam.ac.uk>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(** Paths of documentation *)

module Kind = Paths_types.Kind

(** Identifiers for definitions *)
module Identifier : sig

  include module type of Paths_types.Identifier

  (** {2 Explicit coercions} *)

  val signature_of_module : module_ -> signature

  val signature_of_module_type : module_type -> signature

  val class_signature_of_class : class_ -> class_signature

  val class_signature_of_class_type : class_type -> class_signature

  val datatype_of_type : type_ -> datatype

  val parent_of_signature : signature -> parent

  val parent_of_class_signature : class_signature -> parent

  val parent_of_datatype : datatype -> parent

  val label_parent_of_parent : parent -> label_parent

  val label_parent_of_page : page -> label_parent

  val any : 'kind t -> any

  (** {2 Generic operations} *)

  val equal : 'kind t -> 'kind t -> bool

  val hash : 'kind t -> int

  (** {3 Printing} *)

  val name : 'kind t -> string

  (** {2 Root retrieval} *)

  val signature_root : signature -> Root.t

  val module_root : module_ -> Root.t

  val module_type_root : module_type -> Root.t

  val class_signature_root : class_signature -> Root.t

  val label_parent_root : label_parent -> Root.t
end

(** Normal OCaml paths (i.e. the ones present in types) *)
module rec Path : sig

  module Resolved : sig

    include module type of Paths_types.Resolved_path

    (** {2 Creators} *)

    val ident_module : Identifier.module_ -> [< kind > `Module] t

    val ident_module_type : Identifier.module_type -> [< kind > `ModuleType] t

    val ident_type : Identifier.type_ -> [< kind > `Type] t

    val ident_class : Identifier.class_ -> [< kind > `Class] t

    val ident_class_type : Identifier.class_type -> [< kind > `ClassType] t

    (** {2 Explicit coercion} *)

    val any : 'kind t -> any

    (** {2 Generic operations} *)

    val equal : 'kind t -> 'kind t -> bool

    val hash : 'kind t -> int

    val identifier: 'kind t -> 'kind Identifier.t
    (** [identifier rp] extracts the identifier present at the "root" of [rp]. *)

    val is_hidden : 'kind t -> bool
    (** [is_hidden rp] is [true] when some prefix of [rp] (which is not under a
        [Canonical]) is the [Hidden] constructor.

        [Canonical] are treated specialy because we expect them to rewrite a
        hidden path to a non-hidden one. *)

    val rebase : Identifier.signature -> 'kind t -> 'kind t

    val equal_identifier : 'kind Identifier.t -> 'kind t -> bool
  end

  include module type of Paths_types.Path

  (** {2 Creators} *)

  val ident_module : Identifier.module_ -> [< kind > `Module] t

  val ident_module_type : Identifier.module_type -> [< kind > `ModuleType] t

  val ident_type : Identifier.type_ -> [< kind > `Type] t

  val ident_class : Identifier.class_ -> [< kind > `Class] t

  val ident_class_type : Identifier.class_type -> [< kind > `ClassType] t

  val module_ : module_ -> string -> [< kind > `Module] t

  val apply : module_ -> module_ -> [< kind > `Module] t

  val module_type : module_ -> string -> [< kind > `ModuleType] t

  val type_ : module_ -> string -> [< kind > `Type] t

  val class_ : module_ -> string -> [< kind > `Class] t

  val class_type_ : module_ -> string -> [< kind > `ClassType] t

  (** {2 Explicit coercions} *)

  val any : 'kind t -> any

  val type_of_class_type : class_type -> type_

  (** {2 Generic operations} *)

  val equal : 'kind t -> 'kind t -> bool

  val hash : 'kind t -> int

  val is_hidden : 'kind t -> bool
  (** cf. {!Resolved.is_hidden} *)
end

(** OCaml path fragments for specifying module substitutions *)
module Fragment : sig

  module Resolved : sig

    include module type of Paths_types.Resolved_fragment

    (** {2 Explicit coercions} *)

    val signature_of_module : module_ -> signature

    val any : 'b t -> any

    val any_sort : ('b, 'c) raw -> ('b, sort) raw

    (** {2 Attaching fragments to valid paths} *)

    val path: Path.module_ -> 'b t -> 'b Path.t

    val identifier: Identifier.signature -> 'b t -> 'b Identifier.t

    (** {2 Generic operations} *)

    val equal : 'b t -> 'b t -> bool

    val hash : 'b t -> int

    val split : 'b t -> string * 'b t option

  end

  include module type of Paths_types.Fragment

  (** {2 Explicit coercions} *)

  val signature_of_module : module_ -> signature

  val any_sort : ('b, 'c) raw -> ('b, sort) raw

  val any : 'b t -> any

  (** {2 Attaching fragments to valid paths} *)

  val path: Path.module_ -> 'b t -> 'b Path.t

  (** {2 Generic operations} *)

  val equal : 'b t -> 'b t -> bool

  val hash : 'b t -> int

  val split: 'b t -> string * 'b t option

end

(** References present in documentation comments ([{!Foo.Bar}]) *)
module rec Reference : sig

  module Resolved : sig

    include module type of Paths_types.Resolved_reference

    (** {2 Creators} *)

    val ident_module : Identifier.module_ -> [< kind > `Module] t

    val ident_module_type : Identifier.module_type -> [< kind > `ModuleType] t

    val ident_type : Identifier.type_ -> [< kind > `Type] t

    val ident_constructor : Identifier.constructor -> [< kind > `Constructor] t

    val ident_field : Identifier.field -> [< kind > `Field] t

    val ident_extension : Identifier.extension -> [< kind > `Extension] t

    val ident_exception : Identifier.exception_ -> [< kind > `Exception] t

    val ident_value : Identifier.value -> [< kind > `Value] t

    val ident_class : Identifier.class_ -> [< kind > `Class] t

    val ident_class_type : Identifier.class_type -> [< kind > `ClassType] t

    val ident_method : Identifier.method_ -> [< kind > `Method] t

    val ident_instance_variable : Identifier.instance_variable ->
          [< kind > `InstanceVariable] t

    val ident_label : Identifier.label -> [< kind > `Label] t

    val ident_page : Identifier.page -> [< kind > `Page] t

    (** {2 Explicit coercions} *)

    val signature_of_module : module_ -> signature

    val signature_of_module_type : module_type -> signature

    val class_signature_of_class : class_ -> class_signature

    val class_signature_of_class_type : class_type -> class_signature

    val parent_of_signature : signature -> parent

    val parent_of_class_signature : class_signature -> parent

    val parent_of_datatype : datatype -> parent

    val label_parent_of_parent : parent -> label_parent

    val label_parent_of_page : page -> label_parent

    val any : 'kind t -> any

    (** {2 Generic operations} *)

    val equal : 'kind t -> 'kind t -> bool

    val hash : 'kind t -> int

    val identifier: 'kind t -> 'kind Identifier.t
    (** [identifier rr] extracts the identifier present at the "root" of [rr]. *)

    val rebase : Identifier.signature -> 'kind t -> 'kind t

  end

  include module type of Paths_types.Reference

  (** {2 Creators} *)

  val ident_module : Identifier.module_ -> [< kind > `Module] t

  val ident_module_type : Identifier.module_type -> [< kind > `ModuleType] t

  val ident_type : Identifier.type_ -> [< kind > `Type] t

  val ident_constructor : Identifier.constructor -> [< kind > `Constructor] t

  val ident_field : Identifier.field -> [< kind > `Field] t

  val ident_extension : Identifier.extension -> [< kind > `Extension] t

  val ident_exception : Identifier.exception_ -> [< kind > `Exception] t

  val ident_value : Identifier.value -> [< kind > `Value] t

  val ident_class : Identifier.class_ -> [< kind > `Class] t

  val ident_class_type : Identifier.class_type -> [< kind > `ClassType] t

  val ident_method : Identifier.method_ -> [< kind > `Method] t

  val ident_instance_variable : Identifier.instance_variable ->
        [< kind > `InstanceVariable] t

  val ident_label : Identifier.label -> [< kind > `Label] t

  val module_ : signature -> string -> [< kind > `Module] t

  val module_type : signature -> string ->
        [< kind > `ModuleType] t

  val type_ : signature -> string -> [< kind > `Type] t

  val constructor : datatype -> string -> [< kind > `Constructor] t

  val field : parent -> string -> [< kind > `Field] t

  val extension : signature -> string -> [< kind > `Extension] t

  val exception_ : signature -> string -> [< kind > `Exception] t

  val value : signature -> string -> [< kind > `Value] t

  val class_ : signature -> string -> [< kind > `Class] t

  val class_type : signature -> string -> [< kind > `ClassType] t

  val method_ : class_signature -> string -> [< kind > `Method] t

  val instance_variable : class_signature -> string ->
        [< kind > `InstanceVariable] t

  val label : label_parent -> string -> [< kind > `Label] t

  (** {2 Explicit coercions} *)

  val signature_of_module : module_ -> signature

  val signature_of_module_type : module_type -> signature

  val class_signature_of_class : class_ -> class_signature

  val class_signature_of_class_type : class_type -> class_signature

  val parent_of_signature : signature -> parent

  val parent_of_class_signature : class_signature -> parent

  val parent_of_datatype : datatype -> parent

  val label_parent_of_parent : parent -> label_parent

  val any : 'kind t -> any

  (** {2 Generic operations} *)

  val equal : 'kind t -> 'kind t -> bool

  val hash : 'kind t -> int
end
