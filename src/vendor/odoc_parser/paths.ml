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

module Kind = Paths_types.Kind

open Kind

module Reversed = struct
  type elt =
    | Root of string
    | Module of string
    | ModuleType of string
    | Argument of int * string

  type t = elt list

  let rec remove_prefix prefix ~of_ =
    match prefix, of_ with
    | x1 :: xs1, x2 :: xs2 when x1 = x2 ->
      remove_prefix xs1 ~of_:xs2
    | _, _ -> of_
end

module Identifier = struct

  include Paths_types.Identifier

  let signature_of_module : module_ -> _ = function
    | Root _ | Module _ | Argument _ as x -> x

  let signature_of_module_type : module_type -> _  = function
    | ModuleType _ as x -> x

  let class_signature_of_class : class_ -> _ = function
    | Class _ as x -> x

  let class_signature_of_class_type : class_type -> _ = function
    | ClassType _ as x -> x

  let datatype_of_type : type_ -> datatype = function
    | x -> x

  let parent_of_signature : signature -> parent = function
    | Root _ | Module _ | Argument _ | ModuleType _ as x -> x

  let parent_of_class_signature : class_signature -> parent =
    function Class _ | ClassType _ as x -> x

  let parent_of_datatype : datatype -> parent =
    function Type _ | CoreType _ as x -> x

  let label_parent_of_parent : parent -> label_parent =
    function Root _ | Module _ | Argument _ | ModuleType _ | Type _
           | CoreType _ | Class _ | ClassType _ as x -> x

  let label_parent_of_page : page -> label_parent =
    function Page _ as x -> x

  let any : type k. k t -> any = function
    | Root _ as x -> x
    | Page _ as x -> x
    | Module _ as x -> x
    | Argument _ as x -> x
    | ModuleType _ as x -> x
    | Type _ as x -> x
    | CoreType _ as x -> x
    | Constructor _ as x -> x
    | Field _ as x -> x
    | Extension _ as x -> x
    | Exception _ as x -> x
    | CoreException _ as x -> x
    | Value _ as x -> x
    | Class _ as x -> x
    | ClassType _ as x -> x
    | Method _ as x -> x
    | InstanceVariable _ as x -> x
    | Label _ as x -> x

  let name : type k. k t -> string = function
    | Root(_, name) -> name
    | Page(_, name) -> name
    | Module(_, name) -> name
    | Argument(_, _, name) -> name
    | ModuleType(_, name) -> name
    | Type(_, name) -> name
    | CoreType name -> name
    | Constructor(_, name) -> name
    | Field(_, name) -> name
    | Extension(_, name) -> name
    | Exception(_, name) -> name
    | CoreException name -> name
    | Value(_, name) -> name
    | Class(_, name) -> name
    | ClassType(_, name) -> name
    | Method(_, name) -> name
    | InstanceVariable(_, name) -> name
    | Label(_, name) -> name

  let equal id1 id2 =
    let rec loop : type k. k t -> k t -> bool =
      fun id1 id2 ->
        match id1, id2 with
        | Root(r1, s1), Root(r2, s2) ->
            s1 = s2 && Root.equal r1 r2
        | Module(id1, s1), Module(id2, s2) ->
            s1 = s2 && loop id1 id2
        | Argument(id1, n1, s1), Argument(id2, n2, s2) ->
            n1 = n2 && s1 = s2 && loop id1 id2
        | ModuleType(id1, s1), ModuleType(id2, s2) ->
            s1 = s2 && loop id1 id2
        | Type(id1, s1), Type(id2, s2) ->
            s1 = s2 && loop id1 id2
        | CoreType s1, CoreType s2 ->
            s1 = s2
        | Constructor(id1, s1), Constructor(id2, s2) ->
            s1 = s2 && loop id1 id2
        | Field(id1, s1), Field(id2, s2) ->
            s1 = s2 && loop id1 id2
        | Extension(id1, s1), Extension(id2, s2) ->
            s1 = s2 && loop id1 id2
        | Exception(id1, s1), Exception(id2, s2) ->
            s1 = s2 && loop id1 id2
        | CoreException s1, CoreException s2 ->
            s1 = s2
        | Value(id1, s1), Value(id2, s2) ->
            s1 = s2 && loop id1 id2
        | Class(id1, s1), Class(id2, s2) ->
            s1 = s2 && loop id1 id2
        | ClassType(id1, s1), ClassType(id2, s2) ->
            s1 = s2 && loop id1 id2
        | Method(id1, s1), Method(id2, s2) ->
            s1 = s2 && loop id1 id2
        | InstanceVariable(id1, s1), InstanceVariable(id2, s2) ->
            s1 = s2 && loop id1 id2
        | Label(id1, s1), Label(id2, s2) ->
            s1 = s2 && loop id1 id2
        | _, _ -> false
    in
      loop id1 id2

  let hash id =
    let rec loop : type k. k t -> int =
      fun id ->
        match id with
        | Root(r, s) ->
            Hashtbl.hash (1, Root.hash r, s)
        | Page(r, s) ->
            Hashtbl.hash (2, Root.hash r, s)
        | Module(id, s) ->
            Hashtbl.hash (3, loop id, s)
        | Argument(id, n, s) ->
            Hashtbl.hash (4, loop id, n, s)
        | ModuleType(id, s) ->
            Hashtbl.hash (5, loop id, s)
        | Type(id, s) ->
            Hashtbl.hash (6, loop id, s)
        | CoreType s ->
            Hashtbl.hash (7, s)
        | Constructor(id, s) ->
            Hashtbl.hash (8, loop id, s)
        | Field(id, s) ->
            Hashtbl.hash (9, loop id, s)
        | Extension(id, s) ->
            Hashtbl.hash (10, loop id, s)
        | Exception(id, s) ->
            Hashtbl.hash (11, loop id, s)
        | CoreException s ->
            Hashtbl.hash (12, s)
        | Value(id, s) ->
            Hashtbl.hash (13, loop id, s)
        | Class(id, s) ->
            Hashtbl.hash (14, loop id, s)
        | ClassType(id, s) ->
            Hashtbl.hash (15, loop id, s)
        | Method(id, s) ->
            Hashtbl.hash (16, loop id, s)
        | InstanceVariable(id, s) ->
            Hashtbl.hash (17, loop id, s)
        | Label(id, s) ->
            Hashtbl.hash (18, loop id, s)
    in
      loop id

  let rec signature_root : signature -> Root.t = function
    | Root(r, _) -> r
    | Module(id, _) -> signature_root id
    | Argument(id, _, _) -> signature_root id
    | ModuleType(id, _) -> signature_root id

  let module_root : module_ -> Root.t = function
    | Root(r, _) -> r
    | Module(id, _) -> signature_root id
    | Argument(id, _, _) -> signature_root id

  let module_type_root : module_type -> Root.t = function
    | ModuleType(id, _) -> signature_root id

  let class_signature_root : class_signature -> Root.t = function
    | Class(id, _)
    | ClassType(id, _) -> signature_root id

  let label_parent_root : label_parent -> Root.t = function
    | Root (r, _) -> r
    | Page (r, _) -> r
    | Module (s, _) -> signature_root s
    | Argument (s, _, _) -> signature_root s
    | ModuleType (s, _) -> signature_root s
    | Type (s, _) -> signature_root s
    | CoreType _ -> assert false
    | Class (s, _) -> signature_root s
    | ClassType (s, _) -> signature_root s

  let to_reversed i =
    let rec loop acc : signature -> Reversed.t = function
      | Root (_, s) -> Reversed.Root s :: acc
      | Module (i, s) -> loop (Reversed.Module s :: acc) i
      | ModuleType (i, s) -> loop (Reversed.ModuleType s :: acc) i
      | Argument (i, d, s) -> loop (Reversed.Argument (d, s) :: acc) i
    in
    loop [] i
end



module Path = struct

  (* Separate types module to avoid repeating type definitions *)
  module rec Types : sig

    module Resolved = Paths_types.Resolved_path

    module Path = Paths_types.Path

  end = Types

  let rec equal_resolved_path : type k. k Types.Resolved.t -> k Types.Resolved.t -> bool =
    fun p1 p2 ->
      let open Types.Resolved in
        match p1, p2 with
        | Identifier id1, Identifier id2 ->
            Identifier.equal id1 id2
        | Subst(sub1, p1), Subst(sub2, p2) ->
            equal_resolved_path p1 p2
            && equal_resolved_path sub1 sub2
        | SubstAlias(sub1, p1), SubstAlias(sub2, p2) ->
            equal_resolved_path p1 p2
            && equal_resolved_path sub1 sub2
        | Module(p1, s1), Module(p2, s2) ->
            s1 = s2 && equal_resolved_path p1 p2
        | Apply(p1, arg1), Apply(p2, arg2) ->
            equal_path arg1 arg2
            && equal_resolved_path p1 p2
        | ModuleType(p1, s1), ModuleType(p2, s2) ->
            s1 = s2 && equal_resolved_path p1 p2
        | Type(p1, s1), Type(p2, s2) ->
            s1 = s2 && equal_resolved_path p1 p2
        | Class(p1, s1), Class(p2, s2) ->
            s1 = s2 && equal_resolved_path p1 p2
        | ClassType(p1, s1), ClassType(p2, s2) ->
            s1 = s2 && equal_resolved_path p1 p2
        | _, _ -> false

  and equal_path : type k. k Types.Path.t -> k Types.Path.t -> bool =
    fun p1 p2 ->
      let open Types.Path in
        match p1, p2 with
        | Resolved p1, Resolved p2 ->
            equal_resolved_path p1 p2
        | Root s1, Root s2 ->
            s1 = s2
        | Dot(p1, s1), Dot(p2, s2) ->
            s1 = s2 && equal_path p1 p2
        | Apply(p1, arg1), Apply(p2, arg2) ->
            equal_path arg1 arg2 && equal_path p1 p2
        | _, _ -> false

  let rec hash_resolved_path : type k. k Types.Resolved.t -> int =
    fun p ->
      let open Types.Resolved in
        match p with
        | Identifier id ->
            Identifier.hash id
        | Subst(sub, p) ->
            Hashtbl.hash (19, hash_resolved_path sub,
                          hash_resolved_path p)
        | SubstAlias(sub, p) ->
            Hashtbl.hash (20, hash_resolved_path sub,
                          hash_resolved_path p)
        | Hidden p -> Hashtbl.hash (21, hash_resolved_path p)
        | Module(p, s) ->
            Hashtbl.hash (22, hash_resolved_path p, s)
        | Canonical(p, canonical) ->
          Hashtbl.hash (23, hash_resolved_path p, hash_path canonical)
        | Apply(p, arg) ->
            Hashtbl.hash (24, hash_resolved_path p, hash_path arg)
        | ModuleType(p, s) ->
            Hashtbl.hash (25, hash_resolved_path p, s)
        | Type(p, s) ->
            Hashtbl.hash (26, hash_resolved_path p, s)
        | Class(p, s) ->
            Hashtbl.hash (27, hash_resolved_path p, s)
        | ClassType(p, s) ->
            Hashtbl.hash (28, hash_resolved_path p, s)

  and hash_path : type k. k Types.Path.t -> int =
    fun p ->
      let open Types.Path in
        match p with
        | Resolved p -> hash_resolved_path p
        | Root s ->
            Hashtbl.hash (29, s)
        | Forward s ->
            Hashtbl.hash (30, s)
        | Dot(p, s) ->
            Hashtbl.hash (31, hash_path p, s)
        | Apply(p, arg) ->
            Hashtbl.hash (32, hash_path p, hash_path arg)

  let equal p1 p2 = equal_path p1 p2

  let hash p = hash_path p

  let rec is_resolved_hidden : type k. k Types.Resolved.t -> bool =
    let open Types.Resolved in
    function
    | Identifier _ -> false
    | Canonical (_, _) -> false
    | Hidden _ -> true
    | Subst(p1, p2) -> is_resolved_hidden p1 || is_resolved_hidden p2
    | SubstAlias(p1, p2) -> is_resolved_hidden p1 || is_resolved_hidden p2
    | Module (p, _) -> is_resolved_hidden p
    | Apply (p, _) -> is_resolved_hidden p
    | ModuleType (p, _) -> is_resolved_hidden p
    | Type (p, _) -> is_resolved_hidden p
    | Class (p, _) -> is_resolved_hidden p
    | ClassType (p, _) -> is_resolved_hidden p

  and is_path_hidden : type k. k Types.Path.t -> bool =
    let open Types.Path in
    function
    | Resolved r -> is_resolved_hidden r
    | Root _ -> false
    | Forward _ -> false
    | Dot(p, _) -> is_path_hidden p
    | Apply(p1, p2) -> is_path_hidden p1 || is_path_hidden p2

  module Resolved = struct

    open Identifier

    include Types.Resolved

    let ident_module : Identifier.module_ -> _ = function
      | Root _ | Module _ | Argument _ as x -> Identifier x

    let ident_module_type : Identifier.module_type -> _ = function
      | ModuleType _ as x -> Identifier x

    let ident_type : Identifier.type_ -> _ = function
      | Type _ | CoreType _ as x -> Identifier x

    let ident_class : Identifier.class_ -> _ = function
      | Class _ as x -> Identifier x

    let ident_class_type : Identifier.class_type -> _ = function
      | ClassType _ as x -> Identifier x

    let any : type k. k t -> any = function
      | Identifier (Root _) as x -> x
      | Identifier (Module _) as x -> x
      | Identifier (Argument _) as x -> x
      | Identifier (ModuleType _) as x -> x
      | Identifier (Type _) as x -> x
      | Identifier (CoreType _) as x -> x
      | Identifier (Class _) as x -> x
      | Identifier (ClassType _) as x -> x
      | Subst _ as x -> x
      | SubstAlias _ as x -> x
      | Hidden _ as x -> x
      | Module _ as x -> x
      | Canonical _ as x -> x
      | Apply _ as x -> x
      | ModuleType _ as x -> x
      | Type _ as x -> x
      | Class _ as x -> x
      | ClassType _ as x -> x

    let open_module : 'k. module_ -> ([< kind > `Module ] as 'k) t = function
      | Identifier (Root _ | Module _ | Argument _) | Subst _ | SubstAlias _
      | Hidden _ | Module _ | Canonical _ | Apply _ as x -> x

    let rec parent_module_type_identifier : module_type -> Identifier.signature = function
      | Identifier id -> Identifier.signature_of_module_type id
      | ModuleType(m, n) -> ModuleType(parent_module_identifier m, n)

    and parent_module_identifier : module_ -> Identifier.signature = function
      | Identifier id -> Identifier.signature_of_module id
      | Subst(sub, _) -> parent_module_type_identifier sub
      | SubstAlias(sub, _) -> parent_module_identifier sub
      | Hidden p -> parent_module_identifier p
      | Module(m, n) -> Module(parent_module_identifier m, n)
      | Canonical(_, Types.Path.Resolved p) -> parent_module_identifier p
      | Canonical(p, _) -> parent_module_identifier p
      | Apply(m, _) -> parent_module_identifier m

    let rec identifier : type k. k t -> k Identifier.t = function
      | Identifier id -> id
      | Subst(_, p) -> identifier (open_module p)
      | SubstAlias(_, p) -> identifier (open_module p)
      | Hidden p -> identifier (open_module p)
      | Module(m, n) -> Module(parent_module_identifier m, n)
      | Canonical(_, Types.Path.Resolved p) -> begin
          match identifier p with
          | Root _ | Module _ | Argument _ as x -> x
        end
      | Canonical(p, _) -> begin
          match identifier p with
          | Root _ | Module _ | Argument _ as x -> x
        end
      | Apply(m, _) -> begin
          match identifier m with
          | Root _ | Module _ | Argument _ as x -> x
        end
      | ModuleType(m, n) -> ModuleType(parent_module_identifier m, n)
      | Type(m, n) -> Type(parent_module_identifier m, n)
      | Class(m, n) -> Class(parent_module_identifier m, n)
      | ClassType(m, n) -> ClassType(parent_module_identifier m, n)

    let equal p1 p2 = equal_resolved_path p1 p2

    let hash p = hash_resolved_path p

    type 'kind rebase_result =
      | Stop of 'kind t
      | Continue of 'kind Identifier.t * Reversed.t

    let rec rebase_module_path : Reversed.t -> module_ -> Kind.path_module rebase_result =
      fun new_base t ->
        match t with
        | Identifier id ->
          let rev = Identifier.(to_reversed @@ signature_of_module id) in
          let new_base' = Reversed.remove_prefix rev ~of_:new_base in
          if new_base == new_base' then
            Stop t
          else
            Continue (id, new_base')
        | Subst (_, p)
        | SubstAlias (_, p)
        | Hidden p -> begin
            match rebase_module_path new_base p with
            | Stop p' when p == p' -> Stop t
            | otherwise -> otherwise
          end
        | Module (m, s) ->
          begin match rebase_module_path new_base m with
          | Stop m' -> if m == m' then Stop t else Stop (Module (m', s))
          | Continue (id, new_base) ->
            let id = Identifier.Module(Identifier.signature_of_module id, s) in
            match new_base with
            | Reversed.Module s' :: rest when s = s' ->
              Continue (id, rest)
            | _ ->
                Stop (Identifier id)
          end
        | Canonical (_, Types.Path.Resolved p) ->
          (* We only care about printing at this point, so let's drop the lhs. *)
          rebase_module_path new_base p
        | Canonical (rp, p) ->
          begin match rebase_module_path new_base rp with
          | Stop rp' -> Stop (Canonical (rp', p))
          | _ ->
            (* We might come back at some point with a resolved rhs? So we don't want to
               drop it. *)
            Stop t
          end
        | Apply _ -> Stop t
        (* TODO: rewrite which side? *)

    let rebase : type k. Reversed.t -> k t -> k t =
      fun new_base t ->
        match t with
        | Identifier _ -> t
        | Subst _ -> t (* TODO: rewrite which side? *)
        | SubstAlias _ -> t (* TODO: rewrite which side? *)
        | Hidden p  -> begin
            match rebase_module_path new_base p with
            | Stop p' ->
              if p == p' then t else open_module p'
            | Continue (id, _) -> open_module (Identifier id)
          end
        | Module (mp, s) ->
          begin match rebase_module_path new_base mp with
          | Continue (id, _) ->
            Identifier Identifier.(Module (signature_of_module id, s))
          | Stop mp' -> Module (mp', s)
          end
        | Canonical (p, Types.Path.Resolved rp) ->
          begin match rebase_module_path new_base rp with
          | Continue (id, _) -> ident_module id
          | Stop rp ->
            (* Easier to reexport a canonical than get the type for rp right... *)
            Canonical (p, Types.Path.Resolved rp)
          end
        | Canonical (rp, p) ->
          begin match rebase_module_path new_base rp with
          | Stop rp' -> Canonical (rp', p)
          | _ ->
            (* We might come back at some point with a resolved rhs? So we don't want to
               drop it. *)
            t
          end
        | Apply (mp, arg) ->
          begin match rebase_module_path new_base mp with
          | Continue (id, _) -> Apply (Identifier id, arg)
          | Stop mp' -> Apply (mp', arg)
          end
        | ModuleType (mp, s) ->
          begin match rebase_module_path new_base mp with
          | Continue (id, _) ->
            Identifier Identifier.(ModuleType (signature_of_module id, s))
          | Stop mp' -> ModuleType (mp', s)
          end
        | Type (mp, s) ->
          begin match rebase_module_path new_base mp with
          | Continue (id, _) ->
            Identifier Identifier.(Type (signature_of_module id, s))
          | Stop mp' -> Type (mp', s)
          end
        | Class (mp, s) ->
          begin match rebase_module_path new_base mp with
          | Continue (id, _) ->
            Identifier Identifier.(Class (signature_of_module id, s))
          | Stop mp' -> Class (mp', s)
          end
        | ClassType (mp, s) ->
          begin match rebase_module_path new_base mp with
          | Continue (id, _) ->
            Identifier Identifier.(ClassType (signature_of_module id, s))
          | Stop mp' -> ClassType (mp', s)
          end

    let rebase id t =
      let rev = Identifier.to_reversed id in
      rebase rev t

    let signature_of_module : module_ -> Kind.signature t = function
      | Identifier (Root _) as x -> x
      | Identifier (Module _) as x -> x
      | Identifier (Argument _) as x -> x
      | Module _ as x -> x
      | Canonical _ as x -> x
      | Apply _ as x -> x
      | Hidden _ as x -> x
      | Subst _ as x -> x
      | SubstAlias _ as x -> x

    let rec equal_identifier :
      type k. k Identifier.t -> k t -> bool =
      fun id p ->
        match id, p with
        | _, Identifier id' -> Identifier.equal id id'
        | Module (id, s1), Module (p, s2) when s1 = s2 ->
          equal_identifier id (signature_of_module p)
        | ModuleType (id, s1), ModuleType (p, s2) when s1 = s2 ->
          equal_identifier id (signature_of_module p)
        | _, _ ->
          false


    let is_hidden = is_resolved_hidden
  end

  open Identifier
  open Resolved

  include Types.Path

  let ident_module : Identifier.module_ -> _ = function
    | Root _ | Module _ | Argument _ as x -> Resolved (Identifier x)

  let ident_module_type : Identifier.module_type -> _ = function
    | ModuleType _ as x -> Resolved (Identifier x)

  let ident_type : Identifier.type_ -> _ = function
    | Type _ | CoreType _ as x -> Resolved (Identifier x)

  let ident_class : Identifier.class_ -> _ = function
    | Class _ as x -> Resolved (Identifier x)

  let ident_class_type : Identifier.class_type -> _ = function
    | ClassType _ as x -> Resolved (Identifier x)

  let any : type k. k t -> any = function
    | Resolved (Identifier (Root _)) as x -> x
    | Resolved (Identifier (Module _)) as x -> x
    | Resolved (Identifier (Argument _)) as x -> x
    | Resolved (Identifier (ModuleType _)) as x -> x
    | Resolved (Identifier (Type _)) as x -> x
    | Resolved (Identifier (CoreType _)) as x -> x
    | Resolved (Identifier (Class _)) as x -> x
    | Resolved (Identifier (ClassType _)) as x -> x
    | Resolved (Hidden _) as x -> x
    | Resolved (Module _) as x -> x
    | Resolved (Canonical _) as x -> x
    | Resolved (Apply _) as x -> x
    | Resolved (ModuleType _) as x -> x
    | Resolved (Type _) as x -> x
    | Resolved (Class _) as x -> x
    | Resolved (ClassType _) as x -> x
    | Resolved (Subst _) as x -> x
    | Resolved (SubstAlias _) as x -> x
    | Root _ as x -> x
    | Forward _ as x -> x
    | Dot _ as x -> x
    | Apply _ as x -> x

  let module_ p name =
    match p with
    | Resolved p -> Resolved (Module(p, name))
    | p -> Dot(p, name)

  let apply p arg =
    match p with
    | Resolved p -> Resolved (Apply(p, arg))
    | p -> Apply(p, arg)

  let module_type p name =
    match p with
    | Resolved p -> Resolved (ModuleType(p, name))
    | p -> Dot(p, name)

  let type_ p name =
    match p with
    | Resolved p -> Resolved (Type(p, name))
    | p -> Dot(p, name)

  let class_ p name =
    match p with
    | Resolved p -> Resolved (Class(p, name))
    | p -> Dot(p, name)

  let class_type_ p name =
    match p with
    | Resolved p -> Resolved (ClassType(p, name))
    | p -> Dot(p, name)

  let type_of_class_type : class_type -> type_ = function
    | Resolved (Identifier (Class _)) as x -> x
    | Resolved (Identifier (ClassType _)) as x -> x
    | Resolved (Class _) as x -> x
    | Resolved (ClassType _) as x -> x
    | Dot _ as x -> x

  let is_hidden = is_path_hidden
end



module Fragment = struct

  module Resolved = struct

    include Paths_types.Resolved_fragment

    let signature_of_module : module_ -> signature = function
      | Subst _ | SubstAlias _ | Module _ as x -> x

    let any_sort : type b c. (b, c) raw -> (b, sort) raw =
      function
      | Root as x -> x
      | Subst _ as x -> x
      | SubstAlias _ as x -> x
      | Module (_,_) as x -> x
      | Type (_,_) as x -> x
      | Class (_,_) as x -> x
      | ClassType (_,_) as x -> x

    let open_sort : module_ -> (Kind.fragment_module, [< sort > `Branch ]) raw =
      function
      | Module _ | Subst _ | SubstAlias _ as x -> x

    let open_module : module_ -> ([< kind > `Module ]) t =
      function
      | Module _ | Subst _ | SubstAlias _ as x -> x

    let any : type k. k t -> any = function
      | Subst _ as x -> x
      | SubstAlias _ as x -> x
      | Module _ as x -> x
      | Type _ as x -> x
      | Class _ as x -> x
      | ClassType _ as x -> x

    let rec parent_resolved_path root = function
      | Root -> root
      | Subst(sub, p) ->
          Path.Resolved.Subst(sub, parent_resolved_path root (open_sort p))
      | SubstAlias(sub, p) ->
          Path.Resolved.SubstAlias(sub, parent_resolved_path root (open_sort p))
      | Module(m, n) ->
          Path.Resolved.Module(parent_resolved_path root m, n)

    let rec resolved_path
        : type k. Path.Resolved.module_ ->
               k t -> k Path.Resolved.t =
      fun root frag ->
        match frag with
        | Subst(sub, p) ->
            Path.Resolved.Subst(sub, resolved_path root p)
        | SubstAlias(sub, p) ->
            Path.Resolved.SubstAlias(sub, resolved_path root p)
        | Module(m, n) ->
            Path.Resolved.Module(parent_resolved_path root m, n)
        | Type( m, n) ->
            Path.Resolved.Type(parent_resolved_path root m, n)
        | Class( m, n) ->
            Path.Resolved.Class(parent_resolved_path root m, n)
        | ClassType( m, n) ->
            Path.Resolved.ClassType(parent_resolved_path root m, n)

    let rec parent_unresolved_path root = function
      | Root -> root
      | Subst(_, p) -> parent_unresolved_path root (open_sort p)
      | SubstAlias(_, p) -> parent_unresolved_path root (open_sort p)
      | Module(m, n) -> Path.Dot(parent_unresolved_path root m, n)

    let rec unresolved_path
        : type k. Path.module_ -> k t -> k Path.t =
      fun root -> function
        | Subst(_, p) -> unresolved_path root (open_module p)
        | SubstAlias(_, p) -> unresolved_path root (open_module p)
        | Module(m, n) -> Path.Dot(parent_unresolved_path root m, n)
        | Type( m, n) -> Path.Dot(parent_unresolved_path root m, n)
        | Class( m, n) -> Path.Dot(parent_unresolved_path root m, n)
        | ClassType( m, n) -> Path.Dot(parent_unresolved_path root m, n)

    let parent_path root frag =
      match root with
      | Path.Resolved root -> Path.Resolved (parent_resolved_path root frag)
      | _ -> parent_unresolved_path root frag

    let path (root : Path.module_) frag =
      match root with
      | Path.Resolved root -> Path.Resolved (resolved_path root frag)
      | _ -> unresolved_path root frag

    let rec parent_identifier root = function
      | Root -> root
      | Subst(sub, _) -> Path.Resolved.parent_module_type_identifier sub
      | SubstAlias(sub, _) -> Path.Resolved.parent_module_identifier sub
      | Module(m, n) -> Identifier.Module(parent_identifier root m, n)

    let rec identifier :
      type k. Identifier.signature -> k t -> k Identifier.t =
        fun root -> function
          | Subst(_, p) -> identifier root (open_module p)
          | SubstAlias(_, p) -> identifier root (open_module p)
          | Module(m, n) -> Identifier.Module(parent_identifier root m, n)
          | Type(m, n) -> Identifier.Type(parent_identifier root m, n)
          | Class(m, n) -> Identifier.Class(parent_identifier root m, n)
          | ClassType(m, n) ->
              Identifier.ClassType(parent_identifier root m, n)

    type ('a, 'b) base_name =
      | Base : ('a, [< sort > `Root]) base_name
      | Branch : string * signature -> ('a, [< sort > `Branch]) base_name

    let rec split_parent
            : type s . (fragment_module, s) raw -> ('a, s) base_name =
      function
        | Root -> Base
        | Subst(_, p) -> split_parent (open_sort p)
        | SubstAlias(_, p) -> split_parent (open_sort p)
        | Module(m, name) ->
            match split_parent m with
            | Base -> Branch(name, Root)
            | Branch(base, m) -> Branch(base, Module(m, name))

    let rec split : type k . k t -> string * k t option = function
      | Subst(_, p) -> split (open_module p)
      | SubstAlias(_, p) -> split (open_module p)
      | Module(m, name) -> begin
          match split_parent m with
          | Base -> name, None
          | Branch(base, m)-> base, Some (Module(m, name))
        end
      | Type(m, name) -> begin
          match split_parent m with
          | Base -> name, None
          | Branch(base, m)-> base, Some (Type(m, name))
        end
      | Class(m, name) -> begin
          match split_parent m with
          | Base -> name, None
          | Branch(base, m)-> base, Some (Class(m, name))
        end
      | ClassType(m, name) -> begin
          match split_parent m with
          | Base -> name, None
          | Branch(base, m)-> base, Some (ClassType(m, name))
        end

    let equal p1 p2 =
      let rec loop : type k s. (k, s) raw -> (k, s) raw -> bool =
        fun p1 p2 ->
          match p1, p2 with
          | Root, Root -> true
          | Subst(sub1, p1), Subst(sub2, p2) ->
              Path.Resolved.equal sub1 sub2
              && loop p1 p2
          | SubstAlias(sub1, p1), SubstAlias(sub2, p2) ->
              Path.Resolved.equal sub1 sub2
              && loop p1 p2
          | Module(p1, s1), Module(p2, s2) ->
              s1 = s2 && loop p1 p2
          | Type(p1, s1), Type(p2, s2) ->
              s1 = s2 && loop p1 p2
          | Class(p1, s1), Class(p2, s2) ->
              s1 = s2 && loop p1 p2
          | ClassType(p1, s1), ClassType(p2, s2) ->
              s1 = s2 && loop p1 p2
          | _, _ -> false
      in
        loop p1 p2

    let hash p =
      let rec loop : type k s. (k, s) raw -> int =
        fun p ->
          match p with
          | Root -> Hashtbl.hash 32
          | Subst(sub, p) ->
              Hashtbl.hash (34, Path.Resolved.hash sub, loop p)
          | SubstAlias(sub, p) ->
              Hashtbl.hash (35, Path.Resolved.hash sub, loop p)
          | Module(p, s) ->
              Hashtbl.hash (36, loop p, s)
          | Type(p, s) ->
              Hashtbl.hash (37, loop p, s)
          | Class(p, s) ->
              Hashtbl.hash (38, loop p, s)
          | ClassType(p, s) ->
              Hashtbl.hash (39, loop p, s)
      in
        loop p

  end

  open Resolved

  include Paths_types.Fragment

  let signature_of_module : module_ -> signature = function
    | Resolved(Subst _ | SubstAlias _ | Module _) | Dot _ as x -> x

  let any_sort : type b c. (b, c) raw -> (b, sort) raw = function
    | Resolved r -> Resolved (any_sort r)
    | Dot _ as x -> x

  let any : type k. k t -> any = function
    | Resolved (Subst _) as x -> x
    | Resolved (SubstAlias _) as x -> x
    | Resolved (Module _) as x -> x
    | Resolved (Type _) as x -> x
    | Resolved (Class _) as x -> x
    | Resolved (ClassType _) as x -> x
    | Dot _ as x -> x

  let rec parent_path root = function
    | Resolved r -> Resolved.parent_path root r
    | Dot(m, n) -> Path.Dot(parent_path root m, n)

  let path : type k. Path.module_ -> k t -> k Path.t =
   fun root -> function
    | Resolved r -> Resolved.path root r
    | Dot(m, s) -> Path.Dot(parent_path root m, s)

  type ('a, 'b) base_name =
    | Base : ('a, [< sort > `Root]) base_name
    | Branch : string * signature -> ('a, [< sort > `Branch]) base_name

  let rec split_parent
          : type s . (fragment_module, s) raw -> ('a, s) base_name =
    function
      | Resolved r -> begin
          match Resolved.split_parent r with
          | Base -> Base
          | Branch(base, m) -> Branch(base, Resolved m)
        end
      | Dot(m, name) -> begin
          match split_parent m with
          | Base -> Branch(name, Resolved Root)
          | Branch(base, m) -> Branch(base, Dot(m, name))
        end

  let split : type k . k t -> string * k t option = function
    | Resolved r ->
        let base, m = Resolved.split r in
        let m =
          match m with
          | None -> None
          | Some m -> Some (Resolved m)
        in
          base, m
    | Dot(m, name) ->
        match split_parent m with
        | Base -> name, None
        | Branch(base, m) -> base, Some(Dot(m, name))

  let equal p1 p2 =
    let rec loop : type k s. (k, s) raw -> (k, s) raw -> bool =
      fun p1 p2 ->
        match p1, p2 with
        | Resolved p1, Resolved p2 ->
            Resolved.equal p1 p2
        | Dot(p1, s1), Dot(p2, s2) ->
            s1 = s2 && loop p1 p2
        | _, _ -> false
    in
      loop p1 p2

  let hash p =
    let rec loop : type k s. (k, s) raw -> int =
      fun p ->
        match p with
        | Resolved p -> Resolved.hash p
        | Dot(p, s) ->
            Hashtbl.hash (40, loop p, s)
    in
      loop p

end

module Reference = struct
  module rec Types : sig
    module Resolved = Paths_types.Resolved_reference

    module Reference = Paths_types.Reference
  end = Types

  let rec hash_resolved : type k. k Types.Resolved.t -> int =
    fun p ->
      let open Types.Resolved in
      match p with
      | Identifier id ->
        Identifier.hash id
      | SubstAlias (r1, r2) ->
        Hashtbl.hash (41, Path.Resolved.hash r1, hash_resolved r2)
      | Module(p, s) ->
        Hashtbl.hash (42, hash_resolved p, s)
      | Canonical (rp, p) ->
        Hashtbl.hash (43, hash_resolved rp, hash_reference p)
      | ModuleType(p, s) ->
        Hashtbl.hash (44, hash_resolved p, s)
      | Type(p, s) ->
        Hashtbl.hash (45, hash_resolved p, s)
      | Constructor(p, s) ->
        Hashtbl.hash (46, hash_resolved p, s)
      | Field(p, s) ->
        Hashtbl.hash (47, hash_resolved p, s)
      | Extension(p, s) ->
        Hashtbl.hash (48, hash_resolved p, s)
      | Exception(p, s) ->
        Hashtbl.hash (49, hash_resolved p, s)
      | Value(p, s) ->
        Hashtbl.hash (50, hash_resolved p, s)
      | Class(p, s) ->
        Hashtbl.hash (51, hash_resolved p, s)
      | ClassType(p, s) ->
        Hashtbl.hash (52, hash_resolved p, s)
      | Method(p, s) ->
        Hashtbl.hash (53, hash_resolved p, s)
      | InstanceVariable(p, s) ->
        Hashtbl.hash (54, hash_resolved p, s)
      | Label(p, s) ->
        Hashtbl.hash (55, hash_resolved p, s)

  and hash_reference : type k. k Types.Reference.t -> int =
    fun p ->
      let open Types.Reference in
      match p with
      | Resolved p -> hash_resolved p
      | Root (s, k) -> Hashtbl.hash (56, s, k)
      | Dot (p,s) -> Hashtbl.hash (57, hash_reference p, s)
      | Module (p,s) -> Hashtbl.hash (58, hash_reference p, s)
      | ModuleType (p,s) -> Hashtbl.hash (59, hash_reference p, s)
      | Type (p,s) -> Hashtbl.hash (60, hash_reference p, s)
      | Constructor (p,s) -> Hashtbl.hash (61, hash_reference p, s)
      | Field (p,s) -> Hashtbl.hash (62, hash_reference p, s)
      | Extension (p,s) -> Hashtbl.hash (63, hash_reference p, s)
      | Exception (p,s) -> Hashtbl.hash (64, hash_reference p, s)
      | Value (p,s) -> Hashtbl.hash (65, hash_reference p, s)
      | Class (p,s) -> Hashtbl.hash (66, hash_reference p, s)
      | ClassType (p,s) -> Hashtbl.hash (67, hash_reference p, s)
      | Method (p,s) -> Hashtbl.hash (68, hash_reference p, s)
      | InstanceVariable (p,s) -> Hashtbl.hash (69, hash_reference p, s)
      | Label (p,s) -> Hashtbl.hash (70, hash_reference p, s)

  module Resolved = struct
    open Identifier

    include Types.Resolved

    let ident_module : Identifier.module_ -> _ = function
      | Root _ | Module _ | Argument _ as x -> Identifier x

    let ident_module_type : Identifier.module_type -> _ = function
      | ModuleType _ as x -> Identifier x

    let ident_type : Identifier.type_ -> _ = function
      | Type _ | CoreType _ as x -> Identifier x

    let ident_constructor : Identifier.constructor -> _ = function
      | Constructor _ as x -> Identifier x

    let ident_field : Identifier.field -> _ = function
      | Field _ as x -> Identifier x

    let ident_extension : Identifier.extension -> _ = function
      | Extension _ as x -> Identifier x

    let ident_exception : Identifier.exception_ -> _ = function
      | Exception _ | CoreException _ as x -> Identifier x

    let ident_value : Identifier.value -> _ = function
      | Value _ as x -> Identifier x

    let ident_class : Identifier.class_ -> _ = function
      | Class _ as x -> Identifier x

    let ident_class_type : Identifier.class_type -> _ = function
      | ClassType _ as x -> Identifier x

    let ident_method : Identifier.method_ -> _ = function
      | Method _ as x -> Identifier x

    let ident_instance_variable : Identifier.instance_variable -> _ =
      function InstanceVariable _ as x -> Identifier x

    let ident_label : Identifier.label -> _ = function
      | Label _ as x -> Identifier x

    let ident_page : Identifier.page -> _ = function
      | Page _ as x -> Identifier x

    let signature_of_module : module_ -> _ = function
      | Identifier (Root _ | Module _ | Argument _)
      | SubstAlias _
      | Module _
      | Canonical _ as x -> x

    let signature_of_module_type : module_type -> _ = function
      | Identifier (ModuleType _) | ModuleType _ as x -> x

    let class_signature_of_class : class_ -> _ = function
      | Identifier (Class _) | Class _ as x -> x

    let class_signature_of_class_type : class_type -> _ = function
      | Identifier (Class _ | ClassType _) | Class _ | ClassType _ as x -> x

    let parent_of_signature : signature -> _ = function
      | Identifier (Root _ | Module _ | Argument _ | ModuleType _)
      | SubstAlias _ | Module _ | ModuleType _ | Canonical _ as x -> x

    let parent_of_class_signature : class_signature -> _ =
      function
      | Identifier (Class _ | ClassType _) | Class _ | ClassType _ as x -> x

    let parent_of_datatype : datatype -> _ = function
      | Identifier (Type _ |CoreType _) | Type _ as x -> x

    let label_parent_of_parent : parent -> label_parent = function
      | Identifier (Root _ | Module _ | Argument _ | ModuleType _
                   |Type _ | CoreType _ | Class _ | ClassType _)
      | SubstAlias _ | Module _ | ModuleType _ | Canonical _
      | Type _ | Class _ | ClassType _ as x -> x

    let label_parent_of_page : page -> label_parent = function
      | Identifier Page _ as x -> x

    let any : type k. k t -> any = function
      | Identifier (Root _ ) as x -> x
      | Identifier (Page _ ) as x -> x
      | Identifier (Module _) as x -> x
      | Identifier (Argument _ ) as x -> x
      | Identifier (ModuleType _) as x -> x
      | Identifier (Type _) as x -> x
      | Identifier (CoreType _) as x -> x
      | Identifier (Constructor _) as x -> x
      | Identifier (Field _) as x -> x
      | Identifier (Extension _) as x -> x
      | Identifier (Exception _) as x -> x
      | Identifier (CoreException _) as x -> x
      | Identifier (Value _) as x -> x
      | Identifier (Class _) as x -> x
      | Identifier (ClassType _) as x -> x
      | Identifier (Method _) as x -> x
      | Identifier (InstanceVariable _) as x -> x
      | Identifier (Label _) as x -> x
      | SubstAlias _ as x -> x
      | Module _ as x -> x
      | Canonical _ as x -> x
      | ModuleType _ as x -> x
      | Type _ as x -> x
      | Constructor _ as x -> x
      | Field _ as x -> x
      | Extension _ as x -> x
      | Exception _ as x -> x
      | Value _ as x -> x
      | Class _ as x -> x
      | ClassType _ as x -> x
      | Method _ as x -> x
      | InstanceVariable _ as x -> x
      | Label _ as x -> x

    let open_module : 'b. module_ -> ([< kind > `Module ] as 'b) t =
      function
      | Identifier (Root _ | Module _ | Argument _) | SubstAlias _
      | Module _ | Canonical _ as x -> x

    let rec parent_signature_identifier : signature -> Identifier.signature =
      function
      | Identifier id -> id
      | SubstAlias(sub, _) -> Path.Resolved.parent_module_identifier sub
      | Module(m, n) -> Module(parent_signature_identifier m, n)
      | Canonical(_, Types.Reference.Resolved r) ->
        parent_signature_identifier (open_module r)
      | Canonical (r, _) -> parent_signature_identifier (open_module r)
      | ModuleType(m, s) -> ModuleType(parent_signature_identifier m, s)

    let parent_type_identifier : datatype -> Identifier.datatype =
      function
      | Identifier id -> id
      | Type(sg, s) -> Type(parent_signature_identifier sg, s)

    let parent_class_signature_identifier :
      class_signature -> Identifier.class_signature =
      function
      | Identifier id -> id
      | Class(sg, s) -> Class(parent_signature_identifier sg, s)
      | ClassType(sg, s) -> ClassType(parent_signature_identifier sg, s)

    let rec parent_identifier : parent -> Identifier.parent =
      function
      | Identifier id -> id
      | SubstAlias(sub, _) ->
        Identifier.parent_of_signature
          (Path.Resolved.parent_module_identifier sub)
      | Module(m, n) -> Module(parent_signature_identifier m, n)
      | Canonical(_, Types.Reference.Resolved r) ->
        parent_identifier (open_module r)
      | Canonical (r, _) -> parent_identifier (open_module r)
      | ModuleType(m, s) -> ModuleType(parent_signature_identifier m, s)
      | Type(sg, s) -> Type(parent_signature_identifier sg, s)
      | Class(sg, s) -> Class(parent_signature_identifier sg, s)
      | ClassType(sg, s) -> ClassType(parent_signature_identifier sg, s)

    let rec label_parent_identifier : label_parent -> Identifier.label_parent =
      function
      | Identifier id -> id
      | SubstAlias(sub, _) ->
        Identifier.label_parent_of_parent (
          Identifier.parent_of_signature
            (Path.Resolved.parent_module_identifier sub))
      | Module(m, n) -> Module(parent_signature_identifier m, n)
      | Canonical(_, Types.Reference.Resolved r) ->
        label_parent_identifier (open_module r)
      | Canonical (r, _) -> label_parent_identifier (open_module r)
      | ModuleType(m, s) -> ModuleType(parent_signature_identifier m, s)
      | Type(sg, s) -> Type(parent_signature_identifier sg, s)
      | Class(sg, s) -> Class(parent_signature_identifier sg, s)
      | ClassType(sg, s) -> ClassType(parent_signature_identifier sg, s)

    let rec identifier: type k. k t -> k Identifier.t = function
      | Identifier id -> id
      | SubstAlias(_, p) -> identifier (open_module p)
      | Module(s, n) -> Module(parent_signature_identifier s, n)
      | Canonical(_, Types.Reference.Resolved p) -> begin
          match identifier p with
          | Root _ | Module _ | Argument _ as x -> x
        end
      | Canonical(p, _) -> begin
          match identifier p with
          | Root _ | Module _ | Argument _ as x -> x
        end
      | ModuleType(s, n) -> ModuleType(parent_signature_identifier s, n)
      | Type(s, n) -> Type(parent_signature_identifier s, n)
      | Constructor(s, n) -> Constructor(parent_type_identifier s, n)
      | Field(s, n) -> Field(parent_identifier s, n)
      | Extension(s, n) -> Extension(parent_signature_identifier s, n)
      | Exception(s, n) -> Exception(parent_signature_identifier s, n)
      | Value(s, n) -> Value(parent_signature_identifier s, n)
      | Class(s, n) -> Class(parent_signature_identifier s, n)
      | ClassType(s, n) -> ClassType(parent_signature_identifier s, n)
      | Method(s, n) -> Method(parent_class_signature_identifier s, n)
      | InstanceVariable(s, n) ->
        InstanceVariable(parent_class_signature_identifier s, n)
      | Label(s, n) -> Label (label_parent_identifier s, n)

    let equal r1 r2 =
      let rec loop : type k. k t -> k t -> bool =
        fun id1 id2 ->
          match id1, id2 with
          | Identifier id1, Identifier id2 ->
              Identifier.equal id1 id2
          | Module(r1, s1), Module(r2, s2) ->
              s1 = s2 && loop r1 r2
          | ModuleType(r1, s1), ModuleType(r2, s2) ->
              s1 = s2 && loop r1 r2
          | Type(r1, s1), Type(r2, s2) ->
              s1 = s2 && loop r1 r2
          | Constructor(r1, s1), Constructor(r2, s2) ->
              s1 = s2 && loop r1 r2
          | Field(r1, s1), Field(r2, s2) ->
              s1 = s2 && loop r1 r2
          | Extension(r1, s1), Extension(r2, s2) ->
              s1 = s2 && loop r1 r2
          | Exception(r1, s1), Exception(r2, s2) ->
              s1 = s2 && loop r1 r2
          | Value(r1, s1), Value(r2, s2) ->
              s1 = s2 && loop r1 r2
          | Class(r1, s1), Class(r2, s2) ->
              s1 = s2 && loop r1 r2
          | ClassType(r1, s1), ClassType(r2, s2) ->
              s1 = s2 && loop r1 r2
          | Method(r1, s1), Method(r2, s2) ->
              s1 = s2 && loop r1 r2
          | InstanceVariable(r1, s1), InstanceVariable(r2, s2) ->
              s1 = s2 && loop r1 r2
          | Label(r1, s1), Label(r2, s2) ->
              s1 = s2 && loop r1 r2
          | _, _ -> false
      in
        loop r1 r2

    let hash p = hash_resolved p

    type 'kind rebase_result =
      | Stop of 'kind t
      | Continue of 'kind Identifier.t * Reversed.t

    let rec rebase_module_reference :
      Reversed.t -> module_ -> Kind.reference_module rebase_result =
      fun new_base t ->
        match t with
        | Identifier id ->
          let rev = Identifier.(to_reversed @@ signature_of_module id) in
          let new_base = Reversed.remove_prefix rev ~of_:new_base in
          Continue (id, new_base)
        | SubstAlias _ -> Stop t (* FIXME? *)
        | Module (m, s) ->
          begin match rebase_signature_reference new_base m with
          | Stop m' -> if m == m' then Stop t else Stop (Module (m', s))
          | Continue (id, new_base) ->
            let id = Identifier.Module(id, s) in
            match new_base with
            | Reversed.Module s' :: rest when s = s' ->
              Continue (id, rest)
            | _ ->
              Stop (Identifier id)
          end
        | Canonical (_, Types.Reference.Resolved p) ->
          (* We only care about printing at this point, so let's drop the lhs. *)
          rebase_module_reference new_base (signature_of_module p)
        | Canonical (rp, p) ->
          begin match rebase_module_reference new_base (signature_of_module rp) with
          | Stop rp' -> Stop (Canonical (rp', p))
          | _ ->
            (* We might come back at some point with a resolved rhs? So we don't want to
               drop it. *)
            Stop t
          end

    and rebase_signature_reference :
      Reversed.t -> signature -> Kind.signature rebase_result =
      fun new_base t ->
        match t with
        | Identifier id ->
          let rev = Identifier.(to_reversed id) in
          let new_base = Reversed.remove_prefix rev ~of_:new_base in
          Continue (id, new_base)
        | ModuleType (m, s) ->
          begin match rebase_signature_reference new_base m with
          | Stop m' -> if m == m' then Stop t else Stop (Module (m', s))
          | Continue (id, new_base) ->
            let id = Identifier.ModuleType(id, s) in
            match new_base with
            | Reversed.ModuleType s' :: rest when s = s' ->
              Continue (id, rest)
            | _ ->
              Stop (Identifier id)
          end
        | Module _ | Canonical _ as x ->
          begin match rebase_module_reference new_base x with
          | Stop rp -> Stop (signature_of_module rp)
          | Continue (id, rev) ->
            Continue (Identifier.signature_of_module id, rev)
          end
        | SubstAlias _ -> Stop t (* FIXME? *)

    let rec rebase : type k. Reversed.t -> k t -> k t =
      fun new_base t ->
        match t with
        | Identifier _ -> t
        | SubstAlias _ -> t (* TODO: rewrite necessary? *)
        | Module (mp, s) ->
          begin match rebase_signature_reference new_base mp with
          | Continue (id, _) ->
            Identifier (Identifier.Module(id, s))
          | Stop mp' -> Module (mp', s)
          end
        | Canonical (p, Types.Reference.Resolved rp) ->
          begin match rebase_module_reference new_base (signature_of_module rp) with
          | Continue (id, _) -> ident_module id
          | Stop rp ->
            (* Easier to reexport a canonical than get the type for rp right... *)
            Canonical (p, Types.Reference.Resolved rp)
          end
        | Canonical (rp, p) ->
          begin match rebase_module_reference new_base rp with
          | Stop rp' -> Canonical (rp', p)
          | _ ->
            (* We might come back at some point with a resolved rhs? So we don't want to
               drop it. *)
            t
          end
        | ModuleType (mp, s) ->
          begin match rebase_signature_reference new_base mp with
          | Continue (id, _) ->
            Identifier (Identifier.ModuleType (id, s))
          | Stop mp' -> ModuleType (mp', s)
          end
        | Type (mp, s) ->
          begin match rebase_signature_reference new_base mp with
          | Continue (id, _) ->
            Identifier (Identifier.Type (id, s))
          | Stop mp' -> Type (mp', s)
          end
        | Constructor (parent, s) ->
          Constructor(rebase new_base parent, s)
        | Field (parent, s) ->
          Field(rebase new_base parent, s)
        | Extension (mp, s) ->
          begin match rebase_signature_reference new_base mp with
          | Continue (id, _) ->
            Identifier (Identifier.Extension (id, s))
          | Stop mp' -> Extension (mp', s)
          end
        | Exception (mp, s) ->
          begin match rebase_signature_reference new_base mp with
          | Continue (id, _) ->
            Identifier (Identifier.Exception (id, s))
          | Stop mp' -> Exception (mp', s)
          end
        | Value (mp, s) ->
          begin match rebase_signature_reference new_base mp with
          | Continue (id, _) ->
            Identifier (Identifier.Value (id, s))
          | Stop mp' -> Value (mp', s)
          end
        | Class (mp, s) ->
          begin match rebase_signature_reference new_base mp with
          | Continue (id, _) ->
            Identifier (Identifier.Class (id, s))
          | Stop mp' -> Class (mp', s)
          end
        | ClassType (mp, s) ->
          begin match rebase_signature_reference new_base mp with
          | Continue (id, _) ->
            Identifier (Identifier.ClassType (id, s))
          | Stop mp' -> ClassType (mp', s)
          end
        | Method (mp, s) ->
            Method (rebase new_base mp, s)
        | InstanceVariable (mp, s) ->
            InstanceVariable (rebase new_base mp, s)
        | Label (mp, s) ->
            Label (rebase new_base mp, s)

    let rebase id t =
      let rev = Identifier.to_reversed id in
      rebase rev t
  end

  open Identifier
  open Resolved

  include Types.Reference

  let ident_module : Identifier.module_ -> _ = function
    | Root _ | Module _ | Argument _ as x -> Resolved (Identifier x)

  let ident_module_type : Identifier.module_type -> _ = function
    | ModuleType _ as x -> Resolved (Identifier x)

  let ident_type : Identifier.type_ -> _ = function
    | Type _ | CoreType _ as x -> Resolved (Identifier x)

  let ident_constructor : Identifier.constructor -> _ = function
    | Constructor _ as x -> Resolved (Identifier x)

  let ident_field : Identifier.field -> _ = function
    | Field _ as x -> Resolved (Identifier x)

  let ident_extension : Identifier.extension -> _ = function
    | Extension _ as x -> Resolved (Identifier x)

  let ident_exception : Identifier.exception_ -> _ = function
    | Exception _ | CoreException _ as x -> Resolved (Identifier x)

  let ident_value : Identifier.value -> _ = function
    | Value _ as x -> Resolved (Identifier x)

  let ident_class : Identifier.class_ -> _ = function
    | Class _ as x -> Resolved (Identifier x)

  let ident_class_type : Identifier.class_type -> _ = function
    | ClassType _ as x -> Resolved (Identifier x)

  let ident_method : Identifier.method_ -> _ = function
    | Method _ as x -> Resolved (Identifier x)

  let ident_instance_variable : Identifier.instance_variable -> _ =
    function InstanceVariable _ as x -> Resolved (Identifier x)

  let ident_label : Identifier.label -> _ = function
    | Label _ as x -> Resolved (Identifier x)

  let signature_of_module : module_ -> signature = function
    | Resolved (Identifier (Root _ | Module _ | Argument _)
               | SubstAlias _ | Module _ | Canonical _)
    | Root (_, (TUnknown | TModule))
    | Dot (_, _)
    | Module (_,_) as x -> x

  let signature_of_module_type : module_type -> signature = function
    | Resolved (Identifier (ModuleType _) | ModuleType _)
    | Root (_, (TUnknown | TModuleType))
    | Dot (_, _)
    | ModuleType (_,_) as x -> x

  let class_signature_of_class : class_ -> class_signature = function
    | Resolved (Identifier (Class _) | Class _)
    | Root (_, (TUnknown | TClass))
    | Dot (_, _)
    | Class (_,_) as x -> x

  let class_signature_of_class_type : class_type -> class_signature = function
    | Resolved (Identifier (Class _ | ClassType _) | Class _ | ClassType _)
    | Root (_, (TUnknown | TClass | TClassType))
    | Dot (_, _)
    | Class (_,_)
    | ClassType (_,_) as x -> x

  let parent_of_signature : signature -> parent = function
    | Resolved (Identifier (Root _ | Module _ | Argument _ | ModuleType _)
               | SubstAlias _ | Module _ | ModuleType _ | Canonical _)
    | Root (_, (TUnknown | TModule | TModuleType))
    | Dot (_, _)
    | Module (_,_)
    | ModuleType (_,_) as x -> x

  let parent_of_class_signature : class_signature -> parent = function
    | Resolved (Identifier (Class _ | ClassType _) | Class _ | ClassType _)
    | Root  (_, (TUnknown | TClass | TClassType))
    | Dot (_, _)
    | Class (_,_)
    | ClassType (_,_) as x -> x

  let parent_of_datatype : datatype -> parent = function
    | Resolved (Identifier (Type _ | CoreType _) | Type _)
    | Root (_, (TUnknown | TType))
    | Dot (_, _)
    | Type (_,_) as x -> x

  let label_parent_of_parent : parent -> label_parent = function
    | Resolved (Identifier (Root _ | Module _ | Argument _ | ModuleType _
                           |Type _ | CoreType _ | Class _ | ClassType _)
               | SubstAlias _ | Module _ | ModuleType _ | Canonical _
               | Type _ | Class _ | ClassType _)
    | Root (_, (TUnknown | TModule | TModuleType | TType | TClass | TClassType))
    | Dot (_, _)
    | Module (_,_)
    | ModuleType (_,_)
    | Type (_,_)
    | Class (_,_)
    | ClassType (_,_)
      as x -> x

  let any : type k. k t -> any = function
    | Resolved (Identifier (Root _)) as x -> x
    | Resolved (Identifier (Page _)) as x -> x
    | Resolved (Identifier (Module _)) as x -> x
    | Resolved (Identifier (Argument _)) as x -> x
    | Resolved (Identifier (ModuleType _)) as x -> x
    | Resolved (Identifier (Type _)) as x -> x
    | Resolved (Identifier (CoreType _)) as x -> x
    | Resolved (Identifier (Constructor _)) as x -> x
    | Resolved (Identifier (Field _)) as x -> x
    | Resolved (Identifier (Extension _)) as x -> x
    | Resolved (Identifier (Exception _)) as x -> x
    | Resolved (Identifier (CoreException _)) as x -> x
    | Resolved (Identifier (Value _)) as x -> x
    | Resolved (Identifier (Class _)) as x -> x
    | Resolved (Identifier (ClassType _)) as x -> x
    | Resolved (Identifier (Method _)) as x -> x
    | Resolved (Identifier (InstanceVariable _)) as x -> x
    | Resolved (Identifier (Label _)) as x -> x
    | Resolved (SubstAlias _) as x -> x
    | Resolved (Module _) as x -> x
    | Resolved (Canonical _) as x -> x
    | Resolved (ModuleType _) as x -> x
    | Resolved (Type _) as x -> x
    | Resolved (Constructor _) as x -> x
    | Resolved (Field _) as x -> x
    | Resolved (Extension _) as x -> x
    | Resolved (Exception _) as x -> x
    | Resolved (Value _) as x -> x
    | Resolved (Class _) as x -> x
    | Resolved (ClassType _) as x -> x
    | Resolved (Method _) as x -> x
    | Resolved (InstanceVariable _) as x -> x
    | Resolved (Label _) as x -> x
    | Root (_, TUnknown) as x -> x
    | Root (_, TModule) as x -> x
    | Root (_, TModuleType) as x -> x
    | Root (_, TType) as x -> x
    | Root (_, TConstructor) as x -> x
    | Root (_, TField) as x -> x
    | Root (_, TExtension) as x -> x
    | Root (_, TException) as x -> x
    | Root (_, TClass) as x -> x
    | Root (_, TClassType) as x -> x
    | Root (_, TValue) as x -> x
    | Root (_, TMethod) as x -> x
    | Root (_, TInstanceVariable) as x -> x
    | Root (_, TLabel) as x -> x
    | Root (_, TPage) as x -> x
    | Dot (_, _) as x -> x
    | Module (_,_) as x -> x
    | ModuleType (_,_) as x -> x
    | Type (_,_) as x -> x
    | Constructor (_,_) as x -> x
    | Field (_,_) as x -> x
    | Extension (_,_) as x -> x
    | Exception (_,_) as x -> x
    | Class (_,_) as x -> x
    | ClassType (_,_) as x -> x
    | Value (_,_) as x -> x
    | Method (_,_) as x -> x
    | InstanceVariable (_,_) as x -> x
    | Label (_,_) as x -> x

  let module_ p name =
    match p with
    | Resolved p -> Resolved (Module(p, name))
    | p -> Module(p, name)

  let module_type p name =
    match p with
    | Resolved p -> Resolved (ModuleType(p, name))
    | p -> ModuleType(p, name)

  let type_ p name =
    match p with
    | Resolved p -> Resolved (Type(p, name))
    | p -> Type(p, name)

  let constructor p arg =
    match p with
    | Resolved p -> Resolved (Constructor(p, arg))
    | p -> Constructor(p, arg)

  let field p arg =
    match p with
    | Resolved p -> Resolved (Field(p, arg))
    | p -> Field(p, arg)

  let extension p arg =
    match p with
    | Resolved p -> Resolved (Extension(p, arg))
    | p -> Extension(p, arg)

  let exception_ p arg =
    match p with
    | Resolved p -> Resolved (Exception(p, arg))
    | p -> Exception(p, arg)

  let value p arg =
    match p with
    | Resolved p -> Resolved (Value(p, arg))
    | p -> Value(p, arg)

  let class_ p name =
    match p with
    | Resolved p -> Resolved (Class(p, name))
    | p -> Class(p, name)

  let class_type p name =
    match p with
    | Resolved p -> Resolved (ClassType(p, name))
    | p -> ClassType(p, name)

  let method_ p arg =
    match p with
    | Resolved p -> Resolved (Method(p, arg))
    | p -> Method(p, arg)

  let instance_variable p arg =
    match p with
    | Resolved p -> Resolved (InstanceVariable(p, arg))
    | p -> InstanceVariable(p, arg)

  let label p arg =
    match p with
    | Resolved p -> Resolved (Label(p, arg))
    | p -> Label(p, arg)

  let equal r1 r2 =
    let rec loop : type k. k t -> k t -> bool =
      fun r1 r2 ->
        match r1, r2 with
        | Resolved r1, Resolved r2 ->
            Resolved.equal r1 r2
        | Root (s1, k1), Root (s2, k2) ->
            s1 = s2 && k1 = k2
        | Dot(r1, s1), Dot(r2, s2) ->
            s1 = s2 && loop r1 r2
        | Module(r1, s1), Module(r2, s2) ->
            s1 = s2 && loop r1 r2
        | ModuleType(r1, s1), ModuleType(r2, s2) ->
            s1 = s2 && loop r1 r2
        | Type(r1, s1), Type(r2, s2) ->
            s1 = s2 && loop r1 r2
        | Constructor(r1, s1), Constructor(r2, s2) ->
            s1 = s2 && loop r1 r2
        | Field(r1, s1), Field(r2, s2) ->
            s1 = s2 && loop r1 r2
        | Extension(r1, s1), Extension(r2, s2) ->
            s1 = s2 && loop r1 r2
        | Exception(r1, s1), Exception(r2, s2) ->
            s1 = s2 && loop r1 r2
        | Class(r1, s1), Class(r2, s2) ->
            s1 = s2 && loop r1 r2
        | ClassType(r1, s1), ClassType(r2, s2) ->
            s1 = s2 && loop r1 r2
        | Method(r1, s1), Method(r2, s2) ->
            s1 = s2 && loop r1 r2
        | InstanceVariable(r1, s1), InstanceVariable(r2, s2) ->
            s1 = s2 && loop r1 r2
        | Label(r1, s1), Label(r2, s2) ->
            s1 = s2 && loop r1 r2
        | _, _ -> false
    in
      loop r1 r2

  let hash p = hash_reference p

end
