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

let rec sign = (~isfunc=false) =>
  fun
  | Mty_signature(sg) => sg
  | [@implicit_arity] Mty_functor(_, t, _) when isfunc =>
    switch (t) {
    | None => []
    | Some(t) => sign(t)
    }
  | [@implicit_arity] Mty_functor(_, _, t) => sign(t)
  | _ => [];

let item = maker =>
  fun
  | [@implicit_arity]
    Sig_value({name, _}, {val_loc: {Location.loc_start: loc, _}, _}) => [
      (name, loc),
    ]
  | [@implicit_arity] Sig_type({name: t, _}, {type_kind, _}, _) =>
    switch (type_kind) {
    | [@implicit_arity] Type_record(l, _) =>
      List.map(
        ({Types.ld_id: {name, _}, ld_loc: {Location.loc_start: loc, _}, _}) =>
          (t ++ "." ++ name, loc),
        l,
      )
    | Type_variant(l) =>
      List.map(
        ({Types.cd_id: {name, _}, cd_loc: {Location.loc_start: loc, _}, _}) =>
          (t ++ "." ++ name, loc),
        l,
      )
    | _ => []
    }
  | [@implicit_arity] Sig_module({name, _}, {md_type, _}, _)
  | [@implicit_arity] Sig_modtype({name, _}, {mtd_type: Some(md_type), _}) =>
    List.map(((n, l)) => (name ++ "." ++ n, l), maker(md_type))
  | [@implicit_arity]
    Sig_class({name, _}, {cty_loc: {Location.loc_start: loc, _}, _}, _) => [
      (name ++ "#", loc),
    ]
  | _ => [];

let rec make_content = typ =>
  List.map(item(make_content), sign(typ)) |> List.flatten;

let rec make_arg = typ =>
  List.map(item(make_arg), sign(~isfunc=true, typ)) |> List.flatten;

let expr = m =>
  switch (m.mod_desc) {
  | [@implicit_arity] Tmod_apply(m1, m2, _) =>
    let l1 = make_arg(m1.mod_type) |> List.map(((x, _)) => x);
    let l2 = make_content(m2.mod_type);
    List.iter(
      ((x, loc)) => {
        let is_obj = String.contains(x, '#');
        let is_type = !is_obj && DeadType.is_type(x);
        if ((List.mem(x, l1) || l1 == [])
            && (
              is_obj
              || !is_obj
              && is_type
              && exported(loc)
              || !is_obj
              && !is_type
              && exported(loc)
            )) {
          LocHash.add_set(references, loc, m.mod_loc.Location.loc_start);
        };
      },
      l2,
    );
  | _ => ()
  };