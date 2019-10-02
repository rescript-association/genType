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


let later = ref []
let last = ref []

let met = Hashtbl.create 512


let eom () =
  List.iter (fun f -> f ()) !later;
  later := [];
  Hashtbl.reset met


let add lab expr loc last_loc nb_occur =
  let has_val = match expr.exp_desc with
    | Texp_construct (_, {cstr_name = "None"; _}, _) -> false
    | _ -> true
  in
  let occur =
    let occur =
      if not (Hashtbl.mem nb_occur lab) then Hashtbl.add nb_occur lab 1
      else Hashtbl.find nb_occur lab + 1 |> Hashtbl.replace nb_occur lab;
      Hashtbl.find nb_occur lab
    in ref occur
  in
  let call_site =
    if expr.exp_loc.Location.loc_ghost then last_loc
    else expr.exp_loc.Location.loc_start
  in
  if check_underscore lab then
    let loc = VdNode.find loc lab occur in
    if not (Hashtbl.mem met (last_loc, loc, lab)) then begin
      Hashtbl.add met (last_loc, loc, lab) ();
      opt_args := (loc, lab, has_val, call_site) :: !opt_args
    end


let rec process loc args =

  List.iter       (* treat each arg's expression before all (even if ghost) *)
    (function
      | (_, Some e) -> check e
      | _ -> ())
    args;

  if is_ghost loc then ()   (* Ghostbuster *)
  else begin                              (* else: `begin ... end' for aesthetics *)
    let nb_occur = Hashtbl.create 256 in
    let last_loc = !last_loc in
      (* last_loc fixed to avoid side effects if added to later/last *)
    let add lab expr = add lab expr loc last_loc nb_occur in
    let add = function
      | (Asttypes.Optional lab, Some expr) ->
          if VdNode.is_end loc
          && (let fn = loc.Lexing.pos_fname in fn.[String.length fn - 1] = 'i') then
            last := (fun () -> add lab expr) :: !last
          else if VdNode.is_end loc && !depth > 0 then
            later := (fun () -> add lab expr) :: !later
          else
            add lab expr
      | _ -> ()
    in
    List.iter add args
  end


(* Verify the nature of the argument to detect and treat function applications and uses *)
and check e =
  (* Optional arguments used to match a signature are considered used *)
  let get_sig_args typ =
    let rec loop args typ =
      match typ.desc with
      | Tarrow (Asttypes.Optional _ as arg, _, t, _) ->
          loop ((arg, Some {e with exp_desc = Texp_constant (Asttypes.Const_int 0)})::args) t
      | Tarrow (_, _, t, _)
      | Tlink t -> loop args t
      | _ -> args
    in loop [] typ
  in

  match e.exp_desc with
  | Texp_ident (_, _, {val_loc = {Location.loc_start=loc; _}; _}) ->
      process loc (get_sig_args e.exp_type)
  | Texp_apply (exp, _) ->
      begin match exp.exp_desc with
      | Texp_ident (_, _, {val_loc = {Location.loc_start = loc; loc_ghost}; _})
      | Texp_field (_, _, {lbl_loc = {Location.loc_start = loc; loc_ghost}; _}) ->
        process loc (get_sig_args e.exp_type);
        if not loc_ghost then
          last_loc := loc
      | _ -> ()
      end
  | Texp_let (* Partial application as argument may cut in two parts:
              * let _ = partial in implicit opt_args elimination *)
      ( _,
        [{vb_expr =
            { exp_desc = Texp_apply (
                {exp_desc = Texp_ident (_, _, {val_loc = {Location.loc_start = loc; _}; _}); _},
                _) | Texp_ident(_, _, {val_loc = {Location.loc_start = loc; _}; _});
              _};
          _}],
        { exp_desc = Texp_function { cases =
            [{c_lhs = {pat_desc = Tpat_var (_, _); pat_loc = {loc_ghost = true; _}; _};
              c_rhs = {exp_desc = Texp_apply (_, args); exp_loc = {loc_ghost = true; _}; _}; _}];
            _ };
          exp_loc = {loc_ghost = true; _};_}) ->
      process loc args
  | _ -> ()


let node_build loc expr =
  let rec loop loc expr =
    match expr.exp_desc with
    | Texp_function { arg_label = lab;
                      cases = [{c_lhs = {pat_type; _}; c_rhs = exp; _}]; _ } ->
        DeadType.check_style pat_type expr.exp_loc.Location.loc_start;
        begin match lab with
        | Asttypes.Optional s ->
            if !DeadFlag.optn.print || !DeadFlag.opta.print then
              let opts, next = VdNode.get loc in
              VdNode.update loc (s :: opts, next);
            loop loc exp
        | _ -> () end
    | Texp_apply (exp, _) ->
        begin match exp.exp_desc with
        | Texp_ident (_, _, {val_loc = {Location.loc_start = loc2; _}; _})
        | Texp_field (_, _, {lbl_loc = {Location.loc_start = loc2; _}; _})
          when (!DeadFlag.optn.print || !DeadFlag.opta.print)
          && DeadType.nb_args ~keep:`Opt expr.exp_type > 0 ->
            VdNode.merge_locs loc loc2
        | _ -> ()
        end
    | Texp_ident (_, _, {val_loc = {Location.loc_start = loc2; _}; _})
      when !DeadFlag.optn.print || !DeadFlag.opta.print
      && DeadType.nb_args ~keep:`Opt expr.exp_type > 0 ->
        VdNode.merge_locs loc loc2
    | _ -> ()
  in loop loc expr



                (********   WRAPPING  ********)


let wrap f x y =
  if DeadFlag.(!optn.print || !opta.print) then f x y else ()

let process val_loc args =
  wrap process val_loc args
