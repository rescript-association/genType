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

let dependencies = ref []   (* like the cmt value_dependencies but for types *)



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
            save ld_id ld_loc;
            !DeadLexiFi.export_type ld_loc.Location.loc_start (_TO_STRING_ ld_type)
          )
          l
    | Type_variant l ->
        List.iter (fun {Types.cd_id; cd_loc; _} -> save cd_id cd_loc) l
    | _ -> ()


let collect_references loc exp_loc =
  LocHash.add_set references loc exp_loc


(* Look for bad style typing *)
let rec check_style t loc =
  if !DeadFlag.style.DeadFlag.opt_arg then
    match t.desc with
      | Tlink t -> check_style t loc
      | Tarrow (lab, _, t, _) -> begin
          match lab with
            | Optional lab when check_underscore lab ->
                style :=
                  (!current_src, loc,
                   "val f: ... -> (... -> ?_:_ -> ...) -> ...")
                  :: !style
            | _ -> check_style t loc end
      | _ -> ()


let tstr typ =

  let assoc name loc =
    let path = String.concat "." @@ List.rev @@
      name.Asttypes.txt
      :: typ.typ_name.Asttypes.txt :: !mods
      @ (String.capitalize_ascii (unit !current_src):: [])
    in
    begin try match typ.typ_manifest with
      | Some {ctyp_desc=Ttyp_constr (_, {txt;  _}, _); _} ->
          let loc1 = Hashtbl.find fields
            (String.concat "." @@
              String.capitalize_ascii (unit !current_src)
              :: Longident.flatten txt
              @ (name.Asttypes.txt :: []))
          in
          let loc2 = Hashtbl.find fields path in
          dependencies :=
          (loc2, loc1) :: (loc1, loc) :: !dependencies;
      | _ -> ()
    with _ -> () end;
    try
      let loc1 = Hashtbl.find fields path in
      dependencies := (loc1, loc) :: !dependencies
    with Not_found -> Hashtbl.add fields path loc
  in

  let assoc name loc ctyp =
    assoc name loc;
    !DeadLexiFi.tstr_type typ ctyp
  in

  match typ.typ_kind with
    | Ttype_record l ->
        List.iter
          (fun {Typedtree.ld_name; ld_loc; ld_type; _} ->
            assoc ld_name ld_loc.Location.loc_start (_TO_STRING_ ld_type.ctyp_type)
          )
          l
    | Ttype_variant l ->
        List.iter
          (fun {Typedtree.cd_name; cd_loc; _} -> assoc cd_name cd_loc.Location.loc_start _variant)
          l
    | _ -> ()


let report () = report_basic decs "UNUSED CONSTRUCTORS/RECORD FIELDS" !DeadFlag.typ


                (********   WRAPPING  ********)

let wrap f x =
  if DeadFlag.(!typ.print) then f x else ()

let collect_export path u stock t = wrap (collect_export path u stock) t
let tstr typ = wrap tstr typ
let report () = wrap report ()
