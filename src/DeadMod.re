/***************************************************************************/
/*                                                                         */
/*   Copyright (c) 2014-2016 LexiFi SAS. All rights reserved.              */
/*                                                                         */
/*   This source code is licensed under the MIT License                    */
/*   found in the LICENSE file at the root of this source tree             */
/*                                                                         */
/***************************************************************************/

open Ident;
open Types;
open Typedtree;

open DeadCommon;

let defined: ref(list(string)) = (ref([]): ref(list(string)));

let rec getSignature = (~isfunc=false, moduleType) =>
  switch (moduleType) {
  | Mty_signature(signature) => signature
  | Mty_functor(_, tOpt, _) when isfunc =>
    switch (tOpt) {
    | None => []
    | Some(moduleType) => getSignature(moduleType)
    }
  | Mty_functor(_, _, moduleType) => getSignature(moduleType)
  | _ => []
  };

let item = maker =>
  fun
  | Sig_value({name, _}, {val_loc: {Location.loc_start: pos, _}, _}) => [
      (name, pos),
    ]
  | Sig_type({name: t, _}, {type_kind, _}, _) =>
    switch (type_kind) {
    | Type_record(l, _) =>
      List.map(
        ({Types.ld_id: {name, _}, ld_loc: {Location.loc_start: pos, _}, _}) =>
          (t ++ "." ++ name, pos),
        l,
      )
    | Type_variant(l) =>
      List.map(
        ({Types.cd_id: {name, _}, cd_loc: {Location.loc_start: pos, _}, _}) =>
          (t ++ "." ++ name, pos),
        l,
      )
    | _ => []
    }
  | Sig_module({name, _}, {md_type, _}, _)
  | Sig_modtype({name, _}, {mtd_type: Some(md_type), _}) =>
    List.map(((n, l)) => (name ++ "." ++ n, l), maker(md_type))
  | Sig_class({name, _}, {cty_loc: {Location.loc_start: pos, _}, _}, _) => [
      (name ++ "#", pos),
    ]
  | _ => [];

let rec make_content = moduleType =>
  List.map(item(make_content), getSignature(moduleType)) |> List.flatten;

let rec make_arg = moduleType =>
  List.map(item(make_arg), getSignature(~isfunc=true, moduleType)) |> List.flatten;

let expr = m =>
  switch (m.mod_desc) {
  | Tmod_apply(m1, m2, _) =>
    let l1 = make_arg(m1.mod_type) |> List.map(((x, _)) => x);
    let l2 = make_content(m2.mod_type);
    List.iter(
      ((x, pos)) => {
        let is_obj = String.contains(x, '#');
        let is_type = !is_obj && DeadType.is_type(x);
        if ((List.mem(x, l1) || l1 == [])
            && (is_obj || !is_obj && is_type || !is_obj && !is_type)) {
          PosHash.addSet(references, pos, m.mod_loc.Location.loc_start);
        };
      },
      l2,
    );
  | _ => ()
  };