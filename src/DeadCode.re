/***************************************************************************/
/*                                                                         */
/*   Copyright (c) 2014-2016 LexiFi SAS. All rights reserved.              */
/*                                                                         */
/*   This source code is licensed under the MIT License                    */
/*   found in the LICENSE file at the root of this source tree             */
/*                                                                         */
/***************************************************************************/

/** Dead code anlyzing tool. It only reports unused exported values, constructors/record fields
  and methods by default.
  Options can enable reporting of optional arguments always/never used as bad style of code.
  In addition to selecting which reports are to be displayed, the limit of authorized
  occurences needed to be reported can be selected (default is 0).
  It assumes .mli/.mfi are compiled with -keep-locs and .ml/.mf are compiled with -bin-annot.
 */;

open Types;
open Typedtree;

/********   PROCESSING   ********/

let rec collect_export = (~mod_type=false, path, u, stock: DeadCommon.decs) =>
  fun
  | Sig_value(id, {Types.val_loc})
      when !val_loc.Location.loc_ghost && stock === DeadCommon.decs =>
    DeadCommon.export(path, u, stock, id, val_loc)

  | Sig_type(id, t, _) when stock === DeadCommon.decs =>
    DeadType.collect_export([id, ...path], u, stock, t)

  | (
      Sig_module(id, {Types.md_type: t, _}, _) |
      Sig_modtype(id, {Types.mtd_type: Some(t), _})
    ) as s => {
      let collect =
        switch (s) {
        | Sig_modtype(_) => mod_type
        | _ => true
        };
      if (collect) {
        DeadMod.sign(t)
        |> List.iter(collect_export(~mod_type, [id, ...path], u, stock));
      };
    }
  | _ => ();

let collectValueBinding = (super, self, x) => {
  open Asttypes;
  switch (x) {
  | {
      vb_pat: {
        pat_desc:
          Tpat_var(
            _,
            {loc: {Location.loc_start: loc1, loc_ghost: false, _}, _},
          ),
        _,
      },
      vb_expr: {
        exp_desc:
          Texp_ident(
            _,
            _,
            {val_loc: {Location.loc_start: loc2, loc_ghost: false, _}, _},
          ),
        _,
      },
      _,
    } =>
    DeadCommon.VdNode.merge_locs(loc1, loc2)
  | _ => ()
  };

  super.Tast_mapper.value_binding(self, x);
};

let structure_item = (super, self, i) => {
  open Asttypes;
  switch (i.str_desc) {
  | Tstr_type(_, l) => List.iter(DeadType.tstr, l)
  | Tstr_module({mb_name: {txt, _}, _}) =>
    DeadCommon.mods := [txt, ...DeadCommon.mods^];
    DeadMod.defined :=
      [String.concat(".", List.rev(DeadCommon.mods^)), ...DeadMod.defined^];
  | Tstr_include(i) =>
    let collect_include = signature => {
      let prev_last_loc = DeadCommon.last_loc^;
      List.iter(
        collect_export(
          ~mod_type=true,
          [Ident.create(DeadCommon.getModuleName(DeadCommon.current_src^))],
          DeadCommon.include_,
          DeadCommon.incl,
        ),
        signature,
      );
      DeadCommon.last_loc := prev_last_loc;
    };

    let rec includ = mod_expr =>
      switch (mod_expr.mod_desc) {
      | Tmod_ident(_, _) => collect_include(DeadMod.sign(mod_expr.mod_type))
      | Tmod_structure(structure) => collect_include(structure.str_type)
      | Tmod_unpack(_, mod_type) => collect_include(DeadMod.sign(mod_type))
      | Tmod_functor(_, _, _, mod_expr)
      | Tmod_apply(_, mod_expr, _)
      | Tmod_constraint(mod_expr, _, _, _) => includ(mod_expr)
      };

    includ(i.incl_mod);
  | _ => ()
  };
  let r = super.Tast_mapper.structure_item(self, i);
  switch (i.str_desc) {
  | Tstr_module(_) => DeadCommon.mods := List.tl(DeadCommon.mods^)
  | _ => ()
  };
  r;
};
let colletExpr = (super, self, e) => {
  let exp_loc = e.exp_loc.Location.loc_start;
  open Ident;
  switch (e.exp_desc) {
  | Texp_ident(
      path,
      _,
      {Types.val_loc: {Location.loc_start: loc, loc_ghost: false, _}, _},
    ) =>
    DeadCommon.LocHash.add_set(DeadCommon.references, loc, exp_loc)

  | Texp_field(
      _,
      x,
      {lbl_loc: {Location.loc_start: loc, loc_ghost: false, _}, _},
    )
  | Texp_construct(
      x,
      {cstr_loc: {Location.loc_start: loc, loc_ghost: false, _}, _},
      _,
    ) =>
    DeadType.collect_references(loc, exp_loc)

  | _ => ()
  };
  super.Tast_mapper.expr(self, e);
};

/* Traverse the AST */
let collectReferences = {
  /* Tast_mapper */
  let super = Tast_mapper.default;
  let wrap = (f, ~getLoc, ~self, x) => {
    let lastLoc = DeadCommon.last_loc^;
    let thisLoc = getLoc(x).Location.loc_start;
    if (thisLoc != Lexing.dummy_pos) {
      DeadCommon.last_loc := thisLoc;
    };
    let r = f(super, self, x);
    DeadCommon.last_loc := lastLoc;
    r;
  };

  let expr = (self, e) =>
    e |> wrap(colletExpr, ~getLoc=x => x.exp_loc, ~self);
  let value_binding = (self, vb) =>
    vb |> wrap(collectValueBinding, ~getLoc=x => x.vb_expr.exp_loc, ~self);
  Tast_mapper.{...super, expr, value_binding};
};

/* Merge a location's references to another one's */
let assoc = (decs, (loc1, loc2)) => {
  let fn1 = loc1.Lexing.pos_fname
  and fn2 = loc2.Lexing.pos_fname;
  let is_implem = fn => fn.[String.length(fn) - 1] != 'i';
  let has_iface = fn =>
    fn.[String.length(fn) - 1] == 'i'
    || DeadCommon.getModuleName(fn)
    == DeadCommon.getModuleName(DeadCommon.current_src^)
    && (
      try(Sys.file_exists(fn ++ "i")) {
      | Not_found => false
      }
    );

  let is_iface = (fn, loc) =>
    Hashtbl.mem(decs, loc)
    || DeadCommon.getModuleName(fn)
    != DeadCommon.getModuleName(DeadCommon.current_src^)
    || !(is_implem(fn) && has_iface(fn));

  if (fn1 != DeadCommon.none_ && fn2 != DeadCommon.none_ && loc1 != loc2) {
    if (fn1 != fn2 && is_implem(fn1) && is_implem(fn2)) {
      DeadCommon.LocHash.merge_set(
        DeadCommon.references,
        loc2,
        DeadCommon.references,
        loc1,
      );
    };
    if (is_iface(fn1, loc1)) {
      DeadCommon.LocHash.merge_set(
        DeadCommon.references,
        loc1,
        DeadCommon.references,
        loc2,
      );
      if (is_iface(fn2, loc2)) {
        DeadCommon.LocHash.add_set(DeadCommon.references, loc1, loc2);
      };
    } else {
      DeadCommon.LocHash.merge_set(
        DeadCommon.references,
        loc2,
        DeadCommon.references,
        loc1,
      );
    };
  };
};

let clean = (references, loc) => {
  let fn = loc.Lexing.pos_fname;
  if (fn.[String.length(fn) - 1] != 'i'
      && DeadCommon.getModuleName(fn)
      == DeadCommon.getModuleName(DeadCommon.current_src^)) {
    DeadCommon.LocHash.remove(references, loc);
  };
};

let eom = loc_dep => {
  List.iter(assoc(DeadCommon.decs), loc_dep);
  if (Sys.file_exists(DeadCommon.current_src^ ++ "i")) {
    loc_dep
    |> List.iter(((loc1, loc2)) => {
         clean(DeadCommon.references, loc1);
         clean(DeadCommon.references, loc2);
       });
  };
  DeadCommon.VdNode.eom();
  Hashtbl.reset(DeadCommon.incl);
};

let process_signature = (fn, signature: Types.signature) => {
  let module_name = DeadCommon.getModuleName(fn);
  let module_id = Ident.create(String.capitalize_ascii(module_name));
  signature
  |> List.iter(sig_item =>
       collect_export([module_id], module_name, DeadCommon.decs, sig_item)
     );
  DeadCommon.last_loc := Lexing.dummy_pos;
};

let process_structure =
    (cmt_value_dependencies, structure: Typedtree.structure) => {
  cmt_value_dependencies
  |> List.iter(
       fun
       | (
           {
             Types.val_loc: {Location.loc_start: loc1, loc_ghost: false, _},
             _,
           },
           {Types.val_loc: {Location.loc_start: loc2, loc_ghost: false}, _},
         ) => {
           DeadCommon.VdNode.merge_locs(~force=true, loc2, loc1);
         }
       | _ => (),
     );

  structure
  |> collectReferences.Tast_mapper.structure(collectReferences)
  |> ignore;

  cmt_value_dependencies
  |> List.rev_map(((vd1, vd2)) =>
       (
         vd1.Types.val_loc.Location.loc_start,
         vd2.Types.val_loc.Location.loc_start,
       )
     )
  |> eom;
};

/* Starting point */
let load_file =
    (
      ~exportedValuesSignature,
      ~exportedValuesStructure,
      ~sourceFile,
      cmtFilePath,
    ) => {
  DeadCommon.last_loc := Lexing.dummy_pos;
  if (DeadCommon.verbose) {
    GenTypeCommon.logItem("Scanning %s\n", cmtFilePath);
  };
  DeadCommon.current_src := sourceFile;
  let {Cmt_format.cmt_annots, cmt_value_dependencies} =
    Cmt_format.read_cmt(cmtFilePath);
  switch (cmt_annots) {
  | Interface(signature) =>
    exportedValuesSignature(signature);
    process_signature(cmtFilePath, signature.sig_type);
  | Implementation(structure) =>
    let cmtiExists =
      Sys.file_exists((cmtFilePath |> Filename.chop_extension) ++ ".cmti");
    if (!cmtiExists) {
      exportedValuesStructure(structure);
    };
    process_structure(cmt_value_dependencies, structure);
    if (!cmtiExists) {
      process_signature(cmtFilePath, structure.str_type);
    };
  | _ => ()
  };
};

let report = (~locExportedToJS, ~onUnusedValue) => {
  let onItem = (loc, path) => {
    DeadCommon.prloc(loc);
    print_string(path);
    print_newline();
  };
  let onUnusedValue = (loc, path) => {
    onItem(loc, path);
    onUnusedValue(loc, path);
  };
  Printf.printf("\n%s:\n", "UNUSED EXPORTED VALUES");
  DeadCommon.decs
  |> DeadCommon.report(~locExportedToJS, ~onItem=onUnusedValue);
  Printf.printf("\n%s:\n", "UNUSED CONSTRUCTORS/RECORD FIELDS");
  DeadType.decs |> DeadCommon.report(~onItem);
};