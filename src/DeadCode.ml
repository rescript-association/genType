(***************************************************************************)
(*                                                                         *)
(*   Copyright (c) 2014-2016 LexiFi SAS. All rights reserved.              *)
(*                                                                         *)
(*   This source code is licensed under the MIT License                    *)
(*   found in the LICENSE file at the root of this source tree             *)
(*                                                                         *)
(***************************************************************************)

(** Dead code anlyzing tool. It only reports unused exported values, constructors/record fields
  and methods by default.
  Options can enable reporting of optional arguments always/never used as bad style of code.
  In addition to selecting which reports are to be displayed, the limit of authorized
  occurences needed to be reported can be selected (default is 0).
  It assumes .mli/.mfi are compiled with -keep-locs and .ml/.mf are compiled with -bin-annot.
 *)


open Types
open Typedtree

open DeadCommon


                (********   ATTRIBUTES   ********)

let main_files = Hashtbl.create 256   (* names -> paths *)


                (********   PROCESSING   ********)

let rec collect_export ?(mod_type = false) path u stock = function

  | Sig_value (id, ({Types.val_loc; val_type; _} as value))
    when not val_loc.Location.loc_ghost && stock == decs ->
      export path u stock id val_loc;
      let path = Ident.{id with name = id.name ^ "*"} :: path in
      DeadObj.collect_export path u stock ~obj:val_type val_loc;
      !DeadLexiFi.sig_value value

  | Sig_type (id, t, _) when stock == decs ->
      DeadType.collect_export (id :: path) u stock t

  | Sig_class (id, {Types.cty_type = t; cty_loc = loc; _}, _) ->
      DeadObj.collect_export (id :: path) u stock ~cltyp:t loc

  | (Sig_module (id, {Types.md_type = t; _}, _)
  | Sig_modtype (id, {Types.mtd_type = Some t; _})) as s ->
      let collect = match s with Sig_modtype _ -> mod_type | _ -> true in
      if collect then
        DeadMod.sign t
        |> List.iter (collect_export ~mod_type (id :: path) u stock)

  | _ -> ()


let rec treat_exp exp args =
  match exp.exp_desc with
  | Texp_apply (exp, in_args) -> treat_exp exp (in_args @ args)

  | Texp_ident (_, _, {Types.val_loc = {Location.loc_start = loc; _}; _})
  | Texp_field (_, _, {lbl_loc = {Location.loc_start = loc; _}; _}) ->
      DeadArg.process loc args;

  | Texp_match (_, l1, l2, _) ->
      List.iter (fun {c_rhs = exp; _} -> treat_exp exp args) l1;
      List.iter (fun {c_rhs = exp; _} -> treat_exp exp args) l2

  | Texp_ifthenelse (_, exp_then, exp_else) ->
      treat_exp exp_then args;
      begin match exp_else with
      | Some exp -> treat_exp exp args
      | _ -> ()
      end

  | _ -> ()


let value_binding super self x =
  let old_later = !DeadArg.later in
  DeadArg.later := [];
  incr depth;
  let open Asttypes in
  begin match x with
  | { vb_pat =
        { pat_desc = Tpat_var (
            _,
            {loc = {Location.loc_start = loc1; loc_ghost = false; _}; _});
          _};
      vb_expr =
        { exp_desc = Texp_ident (
            _,
            _,
            {val_loc = {Location.loc_start = loc2; loc_ghost = false; _}; _});
          _};
      _
    } ->
      VdNode.merge_locs loc1 loc2
  | { vb_pat =
        { pat_desc = Tpat_var (
            _,
            {loc = {Location.loc_start = loc; loc_ghost = false; _}; _});
          _};
      vb_expr = exp;
      _
    } ->
      DeadArg.node_build loc exp;
      DeadObj.add_var loc exp
  | _ -> ()
  end;

  let r = super.Tast_mapper.value_binding self x in
  List.iter (fun f -> f()) !DeadArg.later;
  DeadArg.later := old_later;
  decr depth;
  r


let structure_item super self i =
  let open Asttypes in
  begin match i.str_desc with
  | Tstr_type  (_, l) ->
      List.iter DeadType.tstr l
  | Tstr_module  {mb_name = {txt; _}; _} ->
      mods := txt :: !mods;
      DeadMod.defined := String.concat "." (List.rev !mods) :: !DeadMod.defined
  | Tstr_class l -> List.iter DeadObj.tstr l
  | Tstr_include i ->
      let collect_include signature =
        let prev_last_loc = !last_loc in
        List.iter
          (collect_export ~mod_type:true [Ident.create (unit !current_src)] _include incl)
          signature;
        last_loc := prev_last_loc;
      in
      let rec includ mod_expr =
        match mod_expr.mod_desc with
        | Tmod_ident (_, _) -> collect_include (DeadMod.sign mod_expr.mod_type)
        | Tmod_structure structure -> collect_include structure.str_type
        | Tmod_unpack (_, mod_type) -> collect_include (DeadMod.sign mod_type)
        | Tmod_functor (_, _, _, mod_expr)
        | Tmod_apply (_, mod_expr, _)
        | Tmod_constraint (mod_expr, _, _, _) -> includ mod_expr
      in
      includ i.incl_mod
  | _ -> ()
  end;
  let r = super.Tast_mapper.structure_item self i in
  begin match i.str_desc with
  | Tstr_module _ -> mods := List.tl !mods
  | _ -> ()
  end;
  r


let pat super self p =
  let pat_loc = p.pat_loc.Location.loc_start in
  let open Asttypes in
  if DeadType.is_unit p.pat_type && !DeadFlag.style.DeadFlag.unit_pat then begin
    match p.pat_desc with
      | Tpat_construct _ -> ()
      | Tpat_var (_, {txt = "eta"; loc = _})
        when p.pat_loc = Location.none -> ()
      | Tpat_var (_, {txt; _})-> ()
      | Tpat_any -> ()
      | _ -> ()
  end;
  begin match p.pat_desc with
  | Tpat_record (l, _) ->
      List.iter
        (fun (_, {Types.lbl_loc = {Location.loc_start = lab_loc; _}; _}, _) ->
          if exported DeadFlag.typ lab_loc then
            DeadType.collect_references lab_loc pat_loc
        )
        l
  | _ -> ()
  end;
  super.Tast_mapper.pat self p


let expr super self e =
  let rec extra = function
    | [] -> ()
    | (Texp_coerce (_, typ), _, _)::l -> DeadObj.coerce e typ.ctyp_type; extra l
    | _::l -> extra l
  in
  extra e.exp_extra;
  let exp_loc = e.exp_loc.Location.loc_start in
  let open Ident in
  begin match e.exp_desc with

  | Texp_ident (path, _, _) when Path.name path = "Mlfi_types.internal_ttype_of" ->
      !DeadLexiFi.ttype_of e

  | Texp_ident (_, _, {Types.val_loc = {Location.loc_start = loc; loc_ghost = false; _}; _})
    when exported DeadFlag.exported loc ->
      LocHash.add_set references loc exp_loc

  | Texp_field (_, _, {lbl_loc = {Location.loc_start = loc; loc_ghost = false; _}; _})
  | Texp_construct (_, {cstr_loc = {Location.loc_start = loc; loc_ghost = false; _}; _}, _)
    when exported DeadFlag.typ loc ->
      DeadType.collect_references loc exp_loc

  | Texp_send (e2, Tmeth_name s, _)
  | Texp_send (e2, Tmeth_val {name = s; _}, _) ->
      DeadObj.collect_references ~meth:s ~call_site:e.exp_loc.Location.loc_start e2


  | Texp_apply (exp, args) ->
      begin match exp.exp_desc with
      | Texp_ident (_, _, {Types.val_loc; _})
        when val_loc.Location.loc_ghost -> (* The node is due to lookup preparation
            * anticipated in the typedtree, wich is a case we do not want to treat
            * otherwise all object's content would be marked as used at this point... *)
          ()
      | Texp_ident (_, _, {val_type; _}) ->
          DeadObj.arg val_type args
      | _ ->
          DeadObj.arg exp.exp_type args
      end

  | _ -> ()
  end;
  super.Tast_mapper.expr self e


(* Parse the AST *)
let collect_references =                          (* Tast_mapper *)
  let super = Tast_mapper.default in
  let wrap f loc self x =
    let l = !last_loc in
    let ll = (loc x).Location.loc_start in
    if ll <> Lexing.dummy_pos then last_loc := ll;
    let prev_last_class = !DeadObj.last_class in
    let r = f self x in
    DeadObj.last_class := prev_last_class;
    last_loc := l;
    r
  in

  let expr = wrap (expr super) (fun x -> x.exp_loc) in
  let pat = wrap (pat super) (fun x -> x.pat_loc) in
  let structure_item = wrap (structure_item super) (fun x -> x.str_loc) in
  let value_binding = wrap (value_binding super) (fun x -> x.vb_expr.exp_loc) in
  let module_expr =
    wrap
      (fun self x -> DeadMod.expr x; super.Tast_mapper.module_expr self x)
      (fun x -> x.mod_loc)
  in
  let class_structure =
    (fun self x ->
     DeadObj.class_structure x; super.Tast_mapper.class_structure self x)
  in
  let class_field =
    (fun self x ->
     DeadObj.class_field x;
     super.Tast_mapper.class_field self x)
  in
  let class_field = wrap class_field (fun x -> x.cf_loc) in
  let typ =
    (fun self x ->
     !DeadLexiFi.type_ext x; super.Tast_mapper.typ self x)
  in
  let type_declaration self x =
    !DeadLexiFi.type_decl x;
    super.Tast_mapper.type_declaration self x
  in
  Tast_mapper.{ super with
                structure_item; expr; pat; value_binding;
                module_expr; class_structure; class_field; typ;
                type_declaration
              }

let starts_with prefix s =
  let len = String.length prefix in
  if len > String.length s then false
  else String.sub s 0 len = prefix

let ends_with suffix s =
  let len = String.length suffix in
  let s_len = String.length s in
  if len > s_len then false
  else String.sub s (s_len - len) len = suffix

let remove_wrap name =
  let n = String.length name in
  let rec loop i =
    if i < n - 1 then
      if name.[i] == '_' && name.[i+1] == '_' then
        String.uncapitalize_ascii (String.sub name (i+2) (n - i - 2))
      else loop (i+1)
    else name
  in loop 0

let regabs fn =
  current_src := fn;
  hashtbl_add_unique_to_list abspath (unit fn) fn;
  hashtbl_add_unique_to_list main_files (unit fn) ()

(* Merge a location's references to another one's *)
let assoc decs (loc1, loc2) =
  let fn1 = loc1.Lexing.pos_fname
  and fn2 = loc2.Lexing.pos_fname in
  let is_implem fn = fn.[String.length fn - 1] <> 'i' in
  let has_iface fn =
    fn.[String.length fn - 1] = 'i'
    ||  ( unit fn = unit !current_src
          &&  try Sys.file_exists (find_abspath fn ^ "i")
              with Not_found -> false
        )
  in
  let is_iface fn loc =
    Hashtbl.mem decs loc || unit fn <> unit !current_src
    || not (is_implem fn && has_iface fn)
  in
  if fn1 <> _none && fn2 <> _none && loc1 <> loc2 then begin
    if (!DeadFlag.internal || fn1 <> fn2) && is_implem fn1 && is_implem fn2 then
      DeadCommon.LocHash.merge_set references loc2 references loc1;
    if is_iface fn1 loc1 then begin
      DeadCommon.LocHash.merge_set references loc1 references loc2;
      if is_iface fn2 loc2 then
        DeadCommon.LocHash.add_set references loc1 loc2
    end
    else
      DeadCommon.LocHash.merge_set references loc2 references loc1
  end


let clean references loc =
  let fn = loc.Lexing.pos_fname in
  if (fn.[String.length fn - 1] <> 'i' && unit fn = unit !current_src) then
    LocHash.remove references loc

let eom loc_dep =
  DeadArg.eom();
  List.iter (assoc decs) loc_dep;
  List.iter (assoc DeadType.decs) !DeadType.dependencies;
  if Sys.file_exists (!current_src ^ "i") then begin
    let clean =
      List.iter
        (fun (loc1, loc2) ->
          clean references loc1; clean references loc2
        )
    in
    clean loc_dep;
    clean !DeadType.dependencies;
  end;
  VdNode.eom ();
  DeadObj.eom ();
  DeadType.dependencies := [];
  Hashtbl.reset incl


let process_signature fn (signature : Types.signature) = 
    let module_name = unit fn in
    let module_id = Ident.create (String.capitalize_ascii module_name) in
    signature |> List.iter(fun sig_item ->
      collect_export [module_id] module_name decs sig_item);
    last_loc := Lexing.dummy_pos

let process_structure cmt_value_dependencies (structure: Typedtree.structure) =
  let prepare = function
    | {Types.val_loc = {Location.loc_start = loc1; loc_ghost = false; _}; _},
      {Types.val_loc = {Location.loc_start = loc2; loc_ghost = false}; _} ->
        VdNode.merge_locs ~force:true loc2 loc1
    | _ -> ()  in
  List.iter prepare cmt_value_dependencies;
  ignore (collect_references.Tast_mapper.structure collect_references structure);
  let loc_dep =
    List.rev_map
      (fun (vd1, vd2) ->
        (vd1.Types.val_loc.Location.loc_start, vd2.Types.val_loc.Location.loc_start)
      )
      cmt_value_dependencies in
  eom loc_dep


(* Starting point *)
let load_file ~exportedValuesSignature ~exportedValuesStructure ~sourceFile cmtFilePath =
  last_loc := Lexing.dummy_pos;
  if !DeadFlag.verbose then Printf.eprintf "Scanning %s\n%!" cmtFilePath;
  regabs sourceFile;
  let {Cmt_format.cmt_annots; cmt_value_dependencies} = Cmt_format.read_cmt cmtFilePath in
  match cmt_annots with
  | Interface signature ->
    process_signature cmtFilePath signature.sig_type;
    exportedValuesSignature(signature)
  | Implementation structure ->
    process_structure cmt_value_dependencies structure;
    let cmtiFilePath = (cmtFilePath |> Filename.chop_extension) ^ ".cmti" in
    if not (Sys.file_exists cmtiFilePath) then
      begin
        process_signature cmtFilePath structure.str_type;
        exportedValuesStructure(structure)
      end
  | _ -> ()


                (********   REPORTING   ********)

(* Prepare the list of opt_args for report *)
let analyze_opt_args () =
  List.iter (fun f -> f ()) !DeadArg.last;
  let all = ref [] in
  let tbl = Hashtbl.create 256 in
  let dec_loc loc = Hashtbl.mem main_files (unit loc.Lexing.pos_fname) in

  let analyze = fun (loc, lab, has_val, call_site) ->
    let slot =
      try Hashtbl.find tbl (loc, lab)
      with Not_found ->
        let r = {with_val = []; without_val = []} in
        if dec_loc loc then begin
          all := (loc, lab, r) :: !all;
          Hashtbl.add tbl (loc, lab) r
        end;
        r
    in
    if has_val then slot.with_val <- call_site :: slot.with_val
    else slot.without_val <- call_site :: slot.without_val
  in

  List.iter analyze !opt_args;
  List.iter                   (* Remove call sites accounted more than once for the same element *)
    (fun (_, _, slot) ->
      slot.with_val     <- List.sort_uniq compare slot.with_val;
      slot.without_val  <- List.sort_uniq compare slot.without_val)
    !all;
  !all


let report_opt_args s l =
  let opt =
    if s = "NEVER" then !DeadFlag.optn
    else !DeadFlag.opta
  in
  let percent = percent opt in
  let rec report_opt_args nb_call =
    let open DeadFlag in
    let l = List.filter
        (fun (_, _, slot, ratio, _) -> let ratio = 1. -. ratio in
          if opt.threshold.optional = `Both then
            ratio >= opt.threshold.percentage && check_length nb_call slot
          else ratio >= percent nb_call
            && (opt.threshold.percentage >= 1. || ratio < (percent (nb_call - 1))))
      @@ List.map
        (fun (loc, lab, slot) ->
          let l = if s = "NEVER" then slot.with_val else slot.without_val in
          let total = List.length slot.with_val + List.length slot.without_val in
          let ratio = float_of_int (List.length l) /. float_of_int total
          in (loc, lab, l, ratio, total))
        l
      |> List.fast_sort (fun (loc1, lab1, slot1, _, _) (loc2, lab2, slot2, _, _) ->
          compare (DeadCommon.abs loc1, loc1, lab1, slot1) (DeadCommon.abs loc2, loc2, lab2, slot2))
    in

    let change =
      let (loc, _, _, _, _) = try List.hd l with _ -> (!last_loc, _none, [], 0., 0) in
      dir (DeadCommon.abs loc)
    in

    let pretty_print = fun (loc, lab, slot, ratio, total) ->
      if change (DeadCommon.abs loc) then print_newline ();
      prloc loc; print_string ("?" ^ lab);
      if ratio <> 0. then begin
        Printf.printf "   (%d/%d calls)" (total - List.length slot) total;
        if opt.call_sites then print_string "  Exceptions:"
      end;
      print_newline ();
      if opt.call_sites then begin
        List.iter (pretty_print_call ()) slot;
        if nb_call <> 0 then print_newline ()
      end
    in

    let continue nb_call =
      opt.threshold.optional = `Both && nb_call < opt.threshold.exceptions
      || opt.threshold.optional = `Percent && percent nb_call > opt.threshold.percentage
    in
    let s =
      (if nb_call > 0 then "OPTIONAL ARGUMENTS: ALMOST "
      else "OPTIONAL ARGUMENTS: ") ^ s
    in
    report s ~opt ~extra:"Except" l continue nb_call pretty_print report_opt_args;

  in report_opt_args 0


let report_unused_exported () =
  report_basic decs "UNUSED EXPORTED VALUES" !DeadFlag.exported

let run () =
  !DeadLexiFi.prepare_report DeadType.decs;
  report_unused_exported();
  (* DeadObj.report(); *)
  (* DeadType.report(); *)
