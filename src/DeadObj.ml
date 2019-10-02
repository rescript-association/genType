(***************************************************************************)
(*                                                                         *)
(*   Copyright (c) 2014-2016 LexiFi SAS. All rights reserved.              *)
(*                                                                         *)
(*   This source code is licensed under the MIT License                    *)
(*   found in the LICENSE file at the root of this source tree             *)
(*                                                                         *)
(***************************************************************************)

open Types
open Typedtree

open DeadCommon



                (********   ATTRIBUTES  ********)

let decs = Hashtbl.create 256

let references = Hashtbl.create 512                 (* references by loc->method->uses *)

let self_ref = Hashtbl.create 512                   (* references by loc->method to itself *)

let content = Hashtbl.create 512                    (* loc -> [field names] *)

let inheritances = Hashtbl.create 512               (* inheritance links loc-> loc *)

let equals = Hashtbl.create 64

let later = ref []

let last_class = ref Lexing.dummy_pos            (* last class met *)

let path2loc = Hashtbl.create 256

let defined = Hashtbl.create 16



                (********   HELPERS   ********)


let repr_loc loc =
  let met = Hashtbl.create 8 in
  let rec loop loc =
    if Hashtbl.mem equals loc then
      let next = Hashtbl.find equals loc in
      if not (Hashtbl.mem met next) then begin
        Hashtbl.add met loc ();
        loop next
      end
      else loc
    else loc
  in
  Hashtbl.add met loc ();
  loop loc


let meth_tbl tbl loc =
  if not (Hashtbl.mem tbl loc) then
    Hashtbl.add tbl loc (Hashtbl.create 16);
  Hashtbl.find tbl loc


let meth_ref = meth_tbl references
let meth_self_ref = meth_tbl self_ref


let add_path path loc =
  Hashtbl.replace path2loc path loc


let get_loc path =
  let path =
    try
      Hashtbl.fold (fun _ (_, path) acc -> path::acc) incl []
      |> find_path path ~sep:'.'
    with Not_found -> try Hashtbl.find defined path with Not_found -> path
  in
  try repr_loc (Hashtbl.find path2loc path) with Not_found -> Lexing.dummy_pos


let know_path path =
  let path = try Hashtbl.find defined path with Not_found -> path in
  Hashtbl.mem path2loc path


let get_method s =
  let rec loop s pos =
    if s.[pos] = '#' then String.sub s (pos + 1) (String.length s - pos - 1)
    else if pos <= 0 then s
    else loop s (pos - 1)
  in loop s (String.length s - 1)


let add_equal loc1 loc2 =
  let loc1 = repr_loc loc1
  and loc2 = repr_loc loc2 in
  if loc1 <> loc2 && not (is_ghost loc1 || is_ghost loc2) then begin
    Hashtbl.replace equals loc1 loc2;
    if loc1 = !last_class then
      last_class := loc2;
    let meths =
      let res = hashtbl_find_list content loc2 in
      if res = [] then hashtbl_find_list content loc1
      else res
    in
    List.iter
      (fun (_, meth) ->
        hashtbl_merge_unique_list (meth_ref loc2) meth (meth_ref loc1) meth;
        hashtbl_merge_unique_list (meth_self_ref loc2) meth (meth_self_ref loc1) meth
      )
      meths;
    hashtbl_merge_unique_list inheritances loc2 inheritances loc1;
    hashtbl_remove_list inheritances loc1;
    hashtbl_remove_list references loc1;
    hashtbl_remove_list decs loc1;
    hashtbl_remove_list content loc1
  end


let rec sign = function
  | Cty_signature sg -> sg
  | Cty_arrow (_, _, t)
  | Cty_constr (_, _, t) -> sign t


let rec treat_fields action typ = match typ.desc with
  | Tobject (t, _)
  | Tarrow (_, _, t, _)
  | Tlink t -> treat_fields action t
  | Tfield (s, k, _, t) ->
      if Btype.field_kind_repr k = Fpresent && s.[0] > 'Z' then
        action s;
      treat_fields action t
  | _ -> ()


let rec repr_exp expr f =
  match expr.exp_desc with
    | Texp_sequence (_, expr)
    | Texp_function { cases = {c_rhs=expr; _}::_ ; _ }
    | Texp_apply (expr, _) -> repr_exp expr f
    | _ -> f expr

let locate expr =
  let locate expr =
    match expr.exp_desc with
    | Texp_instvar (_, _, {Asttypes.loc; _})
    | Texp_new (_, _, {Types.cty_loc=loc; _})
    | Texp_ident (_, _, {Types.val_loc=loc; _}) -> repr_loc loc.Location.loc_start
    | _ -> Lexing.dummy_pos
  in repr_exp expr locate


let eom () =
  Hashtbl.reset defined;
  last_class := Lexing.dummy_pos



                (********   PROCESSING  ********)


let collect_export path u stock ~obj ~cltyp loc =

  let pos = loc.Location.loc_start in

  begin match List.rev_map (fun id -> id.Ident.name) path with
  | h :: t
    when !last_class == Lexing.dummy_pos || !last_class <= pos || decs != incl ->
      let short = String.concat "." t in
      let path = h ^ "." ^ short in
      Hashtbl.add defined short path;
      add_path path pos
  | _ -> ()
  end;

  let stock =
    if stock != DeadCommon.decs then begin
      export (List.tl path) u stock (List.hd path) loc;
      stock
    end
    else begin
      decs
    end
  in

  last_class := pos;


  let save id =
    if not (Sys.file_exists (Filename.chop_extension !current_src ^ ".csml")) then
      export ~sep:"#" path u stock (Ident.create id) loc;
  in

  let typ = match cltyp with
    | None -> obj
    | Some cltyp -> Some (sign cltyp).csig_self
  in
  match typ with
    | Some typ ->
        treat_fields save typ
    | None -> ()


let collect_references ~meth ~call_site expr =

  let loc = locate expr in

  if not (is_ghost loc) then begin
    hashtbl_add_unique_to_list (meth_ref loc) meth call_site;
    if loc = !last_class then
      hashtbl_add_unique_to_list (meth_self_ref loc) meth call_site
  end


let tstr ({ci_expr; ci_decl = {cty_loc = loc; _}; ci_id_name = {txt = name; _}; _}, _) =

  let loc = loc.Location.loc_start in
  last_class := loc;
  let short =
    (List.rev !mods |> String.concat ".")
    ^ (if !mods <> [] then "." else "") ^ name
  in
  let path = String.capitalize_ascii (unit !current_src) ^ "." ^ short in
  if not (Hashtbl.mem defined short) then
    Hashtbl.add defined short path
  else
    let loc =
      if know_path short then get_loc short
      else if know_path path then get_loc path
      else Lexing.dummy_pos
    in
    if loc != Lexing.dummy_pos && loc <> !last_class then
      add_equal !last_class loc
    else
      add_path path !last_class;

  let rec make_dep ci_expr =
    match ci_expr.cl_desc with
    | Tcl_ident (path, _, _) ->
        let loc = get_loc (Path.name path) in
        if loc != Lexing.dummy_pos then
          add_equal !last_class loc
    | Tcl_fun (_, _, _, ci_expr, _)
    | Tcl_constraint (ci_expr, _, _, _, _) -> make_dep ci_expr
    | _ -> ()
  in
  make_dep ci_expr


let add_var loc expr =
  let kind expr =
    match expr.exp_desc with
    | Texp_object _ -> `Obj
    | Texp_new (_, _, {cty_loc = {Location.loc_start = cty_loc; _}; _}) -> `New cty_loc
    | _ -> `Ignore
  in
  match repr_exp expr kind with
  | `Obj ->
      last_class := loc;
  | `New cty_loc -> add_equal loc cty_loc
  | `Ignore -> ()


let class_structure cl_struct =
  let rec add_aliases pat =
    begin match pat.pat_desc with
    | Tpat_alias (_, _, _)
    | Tpat_var (_, _) when not pat.pat_loc.Location.loc_ghost ->
        add_equal pat.pat_loc.Location.loc_start !last_class
    | _ -> () end;
    match pat.pat_desc with
    | Tpat_alias (pat, _, _) -> add_aliases pat
    | _ -> ()
  in
  add_aliases cl_struct.cstr_self


let class_field f =

  let rec locate cl_exp = match cl_exp.cl_desc with
    | Tcl_ident (path, _, _) ->
        let path = Path.name path in
        if Hashtbl.mem defined path then Hashtbl.find defined path
        else path
    | Tcl_fun (_, _, _, cl_exp, _)
    | Tcl_apply (cl_exp, _)
    | Tcl_let (_, _, _, cl_exp)
    | Tcl_open (_, _ ,_, _, cl_exp)
    | Tcl_constraint (cl_exp, _, _, _, _) -> locate cl_exp
    | Tcl_structure _ -> _none
  in

  let update_overr b s =
    let l = hashtbl_find_list content !last_class in
    let l = (b, s) :: List.filter (fun (_, f) -> f <> s) l in
    hashtbl_replace_list content !last_class l
  in

  begin match f.cf_desc with
  | Tcf_inherit (_, cl_exp, _, _, l) ->
      let path = locate cl_exp in
      if path != _none then begin
        hashtbl_add_unique_to_list inheritances !last_class path;
        let loc = get_loc path in
        let equal () = add_equal cl_exp.cl_loc.Location.loc_start (get_loc path) in  (* for uses inside class def *)
        if loc == Lexing.dummy_pos then later := equal :: !later
        else equal ()
      end;
      List.iter (fun (s, _) -> update_overr false s) l

  | Tcf_method ({txt; _}, _, Tcfk_virtual _) ->
      let erase_from_tbl tbl =
        hashtbl_find_list tbl !last_class
        |> List.filter (fun (_, path) -> get_method path <> txt)
        |> hashtbl_replace_list tbl !last_class
      in
      erase_from_tbl content;
  | Tcf_method ({txt; _}, _, _) ->
      update_overr true txt

  | _ -> ()
  end;
  hashtbl_replace_list
    content (repr_loc !last_class)
    (hashtbl_find_list content !last_class)


let arg typ args =
  let apply_to_exp f = function
    | (_, Some exp) :: _ -> f exp
    | _ -> ()
  in
  let rec arg self typ args =
    if args <> [] then match typ.desc with
      | Tlink t -> arg self t args
      | Tarrow (_, t, typ, _) when not self ->
          arg self t [(List.hd args)];
          arg self typ (List.tl args)
      | Tarrow (_, _, typ, _) ->
          arg self typ args
      | Tobject _ when not self ->
          let f exp =
            treat_fields
              (fun s ->
                collect_references ~meth:s ~call_site:exp.exp_loc.Location.loc_start exp
              )
              typ
          in
          apply_to_exp f args
      | Tconstr (_, _, _) ->
          let f exp =
            let loc = locate exp in
            let collect () =
              List.iter
                (fun (_, s) ->
                  collect_references ~meth:s ~call_site:exp.exp_loc.Location.loc_start exp
                )
                (hashtbl_find_list content loc)
            in
            if hashtbl_find_list content loc = [] then
              later := collect :: !later
            else collect ()
          in
          apply_to_exp f args
      | Tvar _ when not self->
          let f exp = arg true exp.exp_type args in
          apply_to_exp f args
      | _ -> ()
  in arg false typ args


let coerce expr typ =
  let loc = locate expr in
  let use meth =
    hashtbl_add_unique_to_list (meth_ref loc) meth expr.exp_loc.Location.loc_start
  in
  treat_fields use typ


let prepare_report () =

  List.iter (fun f -> f ()) !later;

  let apply_self meth loc1 loc2 =
    let loc1 = repr_loc loc1
    and loc2 = repr_loc loc2 in
    hashtbl_merge_unique_list (meth_ref loc2) meth (meth_self_ref loc1) meth;
    hashtbl_merge_unique_list (meth_self_ref loc2) meth (meth_self_ref loc1) meth;
  in

  let move_uses meth loc1 loc2 =
    hashtbl_merge_unique_list (meth_ref loc2) meth (meth_ref loc1) meth;
    hashtbl_remove_list (meth_ref loc1) meth;
  in

  Hashtbl.fold (fun loc _ acc -> loc :: acc) equals []
  |> List.iter
    (fun loc ->
      let dst = repr_loc loc in
      hashtbl_find_list content dst
      |> List.iter (fun (_, meth) -> move_uses meth loc dst)
    );

  let sons =
    Hashtbl.fold
      (fun loc _ acc -> if not (List.mem loc acc) then loc :: acc else acc)
      inheritances
      []
  in

  List.iter
    (fun clas ->
      hashtbl_find_list inheritances clas
      |> List.iter
        (fun paren ->
          let paren = get_loc paren in
          hashtbl_find_list content clas
          |> List.iter (fun (_, meth) -> apply_self meth paren clas)
        )
    )
    sons;

  let spread_ref loc =

    let met = Hashtbl.create 16 in
    let spread clas paren =
      let paren = get_loc paren in
      (hashtbl_find_list content clas)
      |> List.iter
        (function
          | false, meth
            when not (Hashtbl.mem met meth)
            && List.exists (fun (_, meth2) -> meth2 = meth) (hashtbl_find_list content paren) ->
              Hashtbl.add met meth ();
              move_uses meth (repr_loc clas) paren
          | _ -> ()
        )
    in
    List.iter (spread loc) (hashtbl_find_list inheritances loc)
  in
  List.iter spread_ref sons


let report () =
  prepare_report ();

  let cut_main s =
    let rec loop s pos =
      if pos = String.length s then s
      else if s.[pos] = '.' then String.sub s (pos + 1) (String.length s - pos - 1)
      else loop s (pos + 1)
    in loop s 0
  in
  let no_star s =
    let rec loop s pos =
      if pos = String.length s then s
      else if s.[pos] = '*' then
        String.sub s 0 pos ^ String.sub s (pos + 1) (String.length s - pos - 1)
      else loop s (pos + 1)
    in cut_main (loop s 0)
  in

  let folder nb_call = fun loc (fn, path) acc ->

    let exists =
      List.exists
        (fun (overr, meth) -> overr && get_method path = meth)
        (hashtbl_find_list content loc)
    in
    if exists then
      match hashtbl_find_list (meth_ref loc) (get_method path) with
      | exception Not_found when nb_call = 0 ->
          (fn, no_star path, loc, []) :: acc
      | exception Not_found -> acc
      | l when check_length nb_call l -> (fn, no_star path, loc, l) :: acc
      | _ -> acc
    else acc
  in

  report_basic ~folder decs "UNUSED METHODS" !DeadFlag.obj



                (********   WRAPPING  ********)


let wrap f x =
  if !DeadFlag.obj.print then f x else ()

let collect_export path u stock ?obj ?cltyp loc =
  wrap (collect_export path u stock ~obj ~cltyp) loc

let collect_references ~meth ~call_site exp =
  wrap (collect_references ~meth ~call_site) exp

let tstr cl_dec =
  wrap tstr cl_dec

let add_var loc exp =
  wrap (add_var loc) exp

let class_structure cl_struct =
  wrap class_structure cl_struct

let class_field cl_field =
  wrap class_field cl_field

let arg typ args =
  wrap (arg typ) args

let report () =
  wrap report ()
