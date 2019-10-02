(***************************************************************************)
(*                                                                         *)
(*   Copyright (c) 2014-2016 LexiFi SAS. All rights reserved.              *)
(*                                                                         *)
(*   This source code is licensed under the MIT License                    *)
(*   found in the LICENSE file at the root of this source tree             *)
(*                                                                         *)
(***************************************************************************)

open Ident
open Types
open Typedtree

open DeadCommon



let defined : string list ref = ref []


let rec sign ?(isfunc = false) = function
  | Mty_signature sg -> sg
  | Mty_functor (_, t, _) when isfunc -> begin match t with
    | None -> []
    | Some t -> sign t end
  | Mty_functor (_, _, t) -> sign t
  | _ -> []


let item maker = function
  | Sig_value ({name; _}, {val_loc = {Location.loc_start= loc; _}; _}) ->
      (name, loc)::[]
  | Sig_type ({name=t; _}, {type_kind; _}, _) -> begin match type_kind with
    | Type_record (l, _) ->
        List.map
          (fun {Types.ld_id={name; _}; ld_loc = {Location.loc_start = loc; _}; _} ->
            (t ^ "." ^ name, loc)
          )
          l
    | Type_variant l ->
        List.map
          (fun {Types.cd_id={name; _}; cd_loc = {Location.loc_start = loc; _}; _} ->
            (t ^ "." ^ name, loc)
          )
          l
    | _ -> [] end
  | Sig_module ({name; _}, {md_type; _}, _)
  | Sig_modtype ({name; _}, {mtd_type = Some md_type; _}) ->
      List.map (fun (n, l) -> (name ^ "." ^ n, l)) (maker md_type)
  | Sig_class ({name; _}, {cty_loc = {Location.loc_start = loc; _}; _}, _) ->
      (name ^ "#", loc) :: []
  | _ -> []

let rec make_content typ =
  List.map (item make_content) (sign typ)
  |> List.flatten


let rec make_arg typ =
  List.map (item make_arg) (sign ~isfunc:true typ)
  |> List.flatten


let expr m = match m.mod_desc with
  | Tmod_apply (m1, m2, _) ->
      let l1 = make_arg m1.mod_type |> List.map (fun (x, _) -> x) in
      let l2 = make_content m2.mod_type in
      List.iter
        (fun (x, loc) ->
          let is_obj = String.contains x '#' in
          let is_type = not is_obj && DeadType.is_type x in
          if (List.mem x l1 || l1 = [])
          && (is_obj && !DeadFlag.obj.DeadFlag.print
              || not is_obj && is_type && exported DeadFlag.typ loc
              || not is_obj && not is_type && exported DeadFlag.exported loc) then
            LocHash.add_set references loc m.mod_loc.Location.loc_start
        )
        l2
  | _ -> ()


                (********   WRAPPING  ********)

let expr m =
  if [@warning "-44"]
  DeadFlag.(!exported.print || !typ.print || !obj.print) then
    expr m
  else ()
