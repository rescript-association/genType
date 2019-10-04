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


                (********   PROCESSING   ********)

let rec collect_export ?(mod_type = false) path u stock = function

  | Sig_value (id, ({Types.val_loc}))
    when not val_loc.Location.loc_ghost && stock == DeadCommon.decs ->
      DeadCommon.export path u stock id val_loc

  | Sig_type (id, t, _) when stock == DeadCommon.decs ->
      DeadType.collect_export (id :: path) u stock t

  | (Sig_module (id, {Types.md_type = t; _}, _)
  | Sig_modtype (id, {Types.mtd_type = Some t; _})) as s ->
      let collect = match s with Sig_modtype _ -> mod_type | _ -> true in
      if collect then
        DeadMod.sign t
        |> List.iter (collect_export ~mod_type (id :: path) u stock)
  | _ -> ()

let value_binding super self x =
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
      DeadCommon.VdNode.merge_locs loc1 loc2
  | _ -> ()
  end;

  super.Tast_mapper.value_binding self x

let structure_item super self i =
  let open Asttypes in
  begin match i.str_desc with
  | Tstr_type  (_, l) ->
      List.iter DeadType.tstr l
  | Tstr_module  {mb_name = {txt; _}; _} ->
      DeadCommon.mods := txt :: !DeadCommon.mods;
      DeadMod.defined := String.concat "." (List.rev !DeadCommon.mods) :: !DeadMod.defined
  | Tstr_include i ->
      let collect_include signature =
        let prev_last_loc = !DeadCommon.last_loc in
        List.iter
          (collect_export ~mod_type:true [Ident.create (DeadCommon.getModuleName !DeadCommon.current_src)] DeadCommon._include DeadCommon.incl)
          signature;
        DeadCommon.last_loc := prev_last_loc;
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
  | Tstr_module _ -> DeadCommon.mods := List.tl !DeadCommon.mods
  | _ -> ()
  end;
  r
let expr super self e =
  let exp_loc = e.exp_loc.Location.loc_start in
  let open Ident in
  begin match e.exp_desc with

  | Texp_ident (_, _, {Types.val_loc = {Location.loc_start = loc; loc_ghost = false; _}; _})
    when DeadCommon.exported loc ->
      DeadCommon.LocHash.add_set DeadCommon.references loc exp_loc

  | Texp_field (_, _, {lbl_loc = {Location.loc_start = loc; loc_ghost = false; _}; _})
  | Texp_construct (_, {cstr_loc = {Location.loc_start = loc; loc_ghost = false; _}; _}, _)
    when DeadCommon.exported loc ->
      DeadType.collect_references loc exp_loc

  | _ -> ()
  end;
  super.Tast_mapper.expr self e


(* Parse the AST *)
let collect_references =                          (* Tast_mapper *)
  let super = Tast_mapper.default in
  let wrap f loc self x =
    let l = !DeadCommon.last_loc in
    let ll = (loc x).Location.loc_start in
    if ll <> Lexing.dummy_pos then DeadCommon.last_loc := ll;
    let r = f self x in
    DeadCommon.last_loc := l;
    r
  in

  let expr = wrap (expr super) (fun x -> x.exp_loc) in
  let value_binding = wrap (value_binding super) (fun x -> x.vb_expr.exp_loc) in
  Tast_mapper.{ super with expr; value_binding; }

(* Merge a location's references to another one's *)
let assoc decs (loc1, loc2) =
  let fn1 = loc1.Lexing.pos_fname
  and fn2 = loc2.Lexing.pos_fname in
  let is_implem fn = fn.[String.length fn - 1] <> 'i' in
  let has_iface fn =
    fn.[String.length fn - 1] = 'i'
    ||  ( DeadCommon.getModuleName fn = DeadCommon.getModuleName !DeadCommon.current_src
          &&  try Sys.file_exists (DeadCommon.find_abspath fn ^ "i")
              with Not_found -> false
        )
  in
  let is_iface fn loc =
    Hashtbl.mem decs loc || DeadCommon.getModuleName fn <> DeadCommon.getModuleName !DeadCommon.current_src
    || not (is_implem fn && has_iface fn)
  in
  if fn1 <> DeadCommon._none && fn2 <> DeadCommon._none && loc1 <> loc2 then begin
    if (fn1 <> fn2) && is_implem fn1 && is_implem fn2 then
      DeadCommon.LocHash.merge_set DeadCommon.references loc2 DeadCommon.references loc1;
    if is_iface fn1 loc1 then begin
      DeadCommon.LocHash.merge_set DeadCommon.references loc1 DeadCommon.references loc2;
      if is_iface fn2 loc2 then
        DeadCommon.LocHash.add_set DeadCommon.references loc1 loc2
    end
    else
      DeadCommon.LocHash.merge_set DeadCommon.references loc2 DeadCommon.references loc1
  end


let clean references loc =
  let fn = loc.Lexing.pos_fname in
  if (fn.[String.length fn - 1] <> 'i' && DeadCommon.getModuleName fn = DeadCommon.getModuleName !DeadCommon.current_src) then
    DeadCommon.LocHash.remove references loc

let eom loc_dep =
  List.iter (assoc DeadCommon.decs) loc_dep;
  if Sys.file_exists (!DeadCommon.current_src ^ "i") then begin
    let clean =
      List.iter
        (fun (loc1, loc2) ->
          clean DeadCommon.references loc1; clean DeadCommon.references loc2
        )
    in
    clean loc_dep
  end;
  DeadCommon.VdNode.eom ();
  Hashtbl.reset DeadCommon.incl


let process_signature fn (signature : Types.signature) = 
    let module_name = DeadCommon.getModuleName fn in
    let module_id = Ident.create (String.capitalize_ascii module_name) in
    signature |> List.iter(fun sig_item ->
      collect_export [module_id] module_name DeadCommon.decs sig_item);
    DeadCommon.last_loc := Lexing.dummy_pos

let process_structure cmt_value_dependencies (structure: Typedtree.structure) =
  let prepare = function
    | {Types.val_loc = {Location.loc_start = loc1; loc_ghost = false; _}; _},
      {Types.val_loc = {Location.loc_start = loc2; loc_ghost = false}; _} ->
        DeadCommon.VdNode.merge_locs ~force:true loc2 loc1
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
  DeadCommon.last_loc := Lexing.dummy_pos;
  if !DeadFlag.verbose then Printf.eprintf "Scanning %s\n%!" cmtFilePath;
  DeadCommon.current_src := sourceFile;
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

let report () =
  DeadCommon.report DeadCommon.decs "UNUSED EXPORTED VALUES";
  DeadCommon.report DeadType.decs "UNUSED CONSTRUCTORS/RECORD FIELDS"

