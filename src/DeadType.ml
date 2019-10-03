(***************************************************************************)
(*                                                                         *)
(*   Copyright (c) 2014-2016 LexiFi SAS. All rights reserved.              *)
(*                                                                         *)
(*   This source code is licensed under the MIT License                    *)
(*   found in the LICENSE file at the root of this source tree             *)
(*                                                                         *)
(***************************************************************************)

open Asttypes
open Types
open Typedtree

open DeadCommon



                (********   ATTRIBUTES  ********)

let decs = Hashtbl.create 256



                (********   HELPERS   ********)

let is_unit t = match (Ctype.repr t).desc with
  | Tconstr (p, [], _) -> Path.same p Predef.path_unit
  | _ -> false


let nb_args ~keep typ =
  let rec loop n = function
    | Tarrow (_, _, typ, _) when keep = `All -> loop (n + 1) typ.desc
    | Tarrow (Labelled _, _, typ, _) when keep = `Lbl -> loop (n + 1) typ.desc
    | Tarrow (Optional _, _, typ, _) when keep = `Opt -> loop (n + 1) typ.desc
    | Tarrow (Nolabel, _, typ, _) when keep = `Reg -> loop (n + 1) typ.desc
    | Tarrow (_, _, typ, _) -> loop n typ.desc
    | _ -> n
  in
  loop 0 typ.desc


let rec _TO_STRING_ typ = begin [@warning "-11"] match typ.desc with
  | Tvar i -> begin match i with Some id -> id | None -> "'a" end
  | Tarrow (_, t1, t2, _) ->
      begin match t1.desc with
      | Tarrow _ -> "(" ^ _TO_STRING_ t1 ^ ")"
      | _ -> _TO_STRING_ t1 end
      ^ " -> " ^ _TO_STRING_ t2
  | Ttuple l -> begin match l with
      | e::l ->
          List.fold_left (fun prev typ -> prev ^ " * " ^ _TO_STRING_ typ) (_TO_STRING_ e) l
      | [] -> "*" end
  | Tconstr (path, l, _) -> make_name path l
  | Tobject (self, _) -> "< " ^ _TO_STRING_ self ^ " >"
  | Tfield (s, k, _, t1) ->
      if Btype.field_kind_repr k = Fpresent then
        s
        ^ begin match t1.desc with
          | Tfield _ -> "; " ^ _TO_STRING_ t1
          | _ -> "" end
      else _TO_STRING_ t1
  | Tnil -> "Tnil"
  | Tlink t -> _TO_STRING_ t
  | Tsubst _ -> "Tsubst _"
  | Tvariant {row_more; _} -> _TO_STRING_ row_more
  | Tunivar _ -> "Tunivar _"
  | Tpoly (t, _) -> _TO_STRING_ t
  | Tpackage _ -> "Tpackage _"
  | _ -> "Extension _" end


and make_name path l =
  let t = match l with
    | [] -> ""
    | _ -> List.fold_left (fun prev typ -> prev ^ _TO_STRING_ typ ^ " ") "" l;
  in
  let name = Path.name path in
  t ^ name


let is_type s =
  let rec blk s p l acc =
    try
      if s.[p] = '.' then
        let acc = String.sub s (p - l) l :: acc in
        blk s (p + 1) 0 acc
      else blk s (p + 1) (l + 1) acc
    with _ -> String.sub s (p - l) l :: acc
  in
  if not (String.contains s '.') then false
  else
    match blk s 0 0 [] with
    | hd :: cont :: _ ->
      String.capitalize_ascii hd = hd || String.lowercase_ascii cont = cont
    | _ ->
      assert false



                (********   PROCESSING  ********)

let collect_export path u stock t =

  let stock =
    if stock == DeadCommon.decs then decs
    else stock
  in

  let save id loc =
    if t.type_manifest = None then
      export path u stock id loc;
    let path = String.concat "." @@ List.rev_map (fun id -> id.Ident.name) (id::path) in
    Hashtbl.replace fields path loc.Location.loc_start
  in

  match t.type_kind with
    | Type_record (l, _) ->
        List.iter
          (fun {Types.ld_id; ld_loc; ld_type; _} ->
            save ld_id ld_loc
          )
          l
    | Type_variant l ->
        List.iter (fun {Types.cd_id; cd_loc; _} -> save cd_id cd_loc) l
    | _ -> ()


let collect_references declaration_loc usage_loc =
  LocHash.add_set references declaration_loc usage_loc
  

let report () = report_basic decs "UNUSED CONSTRUCTORS/RECORD FIELDS"
