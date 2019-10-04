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
  | [@implicit_arity] Sig_value(id, {Types.val_loc})
      when !val_loc.Location.loc_ghost && stock === DeadCommon.decs =>
    DeadCommon.export(path, u, stock, id, val_loc)

  | [@implicit_arity] Sig_type(id, t, _) when stock === DeadCommon.decs =>
    DeadType.collect_export([id, ...path], u, stock, t)

  | (
      [@implicit_arity] Sig_module(id, {Types.md_type: t, _}, _) |
      [@implicit_arity] Sig_modtype(id, {Types.mtd_type: Some(t), _})
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

let value_binding = (super, self, x) => {
  open Asttypes;
  switch (x) {
  | {
      vb_pat: {
        pat_desc:
          [@implicit_arity]
          Tpat_var(
            _,
            {loc: {Location.loc_start: loc1, loc_ghost: false, _}, _},
          ),
        _,
      },
      vb_expr: {
        exp_desc:
          [@implicit_arity]
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
  | [@implicit_arity] Tstr_type(_, l) => List.iter(DeadType.tstr, l)
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
      | [@implicit_arity] Tmod_ident(_, _) =>
        collect_include(DeadMod.sign(mod_expr.mod_type))
      | Tmod_structure(structure) => collect_include(structure.str_type)
      | [@implicit_arity] Tmod_unpack(_, mod_type) =>
        collect_include(DeadMod.sign(mod_type))
      | [@implicit_arity] Tmod_functor(_, _, _, mod_expr)
      | [@implicit_arity] Tmod_apply(_, mod_expr, _)
      | [@implicit_arity] Tmod_constraint(mod_expr, _, _, _) =>
        includ(mod_expr)
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
let expr = (super, self, e) => {
  let exp_loc = e.exp_loc.Location.loc_start;
  open Ident;
  switch (e.exp_desc) {
  | [@implicit_arity]
    Texp_ident(
      _,
      _,
      {Types.val_loc: {Location.loc_start: loc, loc_ghost: false, _}, _},
    )
      when DeadCommon.exported(loc) =>
    DeadCommon.LocHash.add_set(DeadCommon.references, loc, exp_loc)

  | [@implicit_arity]
    Texp_field(
      _,
      _,
      {lbl_loc: {Location.loc_start: loc, loc_ghost: false, _}, _},
    )
  | [@implicit_arity]
    Texp_construct(
      _,
      {cstr_loc: {Location.loc_start: loc, loc_ghost: false, _}, _},
      _,
    )
      when DeadCommon.exported(loc) =>
    DeadType.collect_references(loc, exp_loc)

  | _ => ()
  };
  super.Tast_mapper.expr(self, e);
};

/* Parse the AST */
let collect_references = {
  /* Tast_mapper */
  let super = Tast_mapper.default;
  let wrap = (f, loc, self, x) => {
    let l = DeadCommon.last_loc^;
    let ll = loc(x).Location.loc_start;
    if (ll != Lexing.dummy_pos) {
      DeadCommon.last_loc := ll;
    };
    let r = f(self, x);
    DeadCommon.last_loc := l;
    r;
  };

  let expr = wrap(expr(super), x => x.exp_loc);
  let value_binding = wrap(value_binding(super), x => x.vb_expr.exp_loc);
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
      try(Sys.file_exists(DeadCommon.find_abspath(fn) ++ "i")) {
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
  let prepare =
    fun
    | (
        {Types.val_loc: {Location.loc_start: loc1, loc_ghost: false, _}, _},
        {Types.val_loc: {Location.loc_start: loc2, loc_ghost: false}, _},
      ) =>
      DeadCommon.VdNode.merge_locs(~force=true, loc2, loc1)
    | _ => ();
  List.iter(prepare, cmt_value_dependencies);
  ignore(
    collect_references.Tast_mapper.structure(collect_references, structure),
  );
  let loc_dep =
    List.rev_map(
      ((vd1, vd2)) =>
        (
          vd1.Types.val_loc.Location.loc_start,
          vd2.Types.val_loc.Location.loc_start,
        ),
      cmt_value_dependencies,
    );
  eom(loc_dep);
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
  if (DeadCommon.verbose^) {
    Printf.eprintf("Scanning %s\n%!", cmtFilePath);
  };
  DeadCommon.current_src := sourceFile;
  let {Cmt_format.cmt_annots, cmt_value_dependencies} =
    Cmt_format.read_cmt(cmtFilePath);
  switch (cmt_annots) {
  | Interface(signature) =>
    process_signature(cmtFilePath, signature.sig_type);
    exportedValuesSignature(signature);
  | Implementation(structure) =>
    process_structure(cmt_value_dependencies, structure);
    let cmtiFilePath = (cmtFilePath |> Filename.chop_extension) ++ ".cmti";
    if (!Sys.file_exists(cmtiFilePath)) {
      process_signature(cmtFilePath, structure.str_type);
      exportedValuesStructure(structure);
    };
  | _ => ()
  };
};

let report = () => {
  DeadCommon.decs |> DeadCommon.report(~title="UNUSED EXPORTED VALUES");
  DeadType.decs
  |> DeadCommon.report(~title="UNUSED CONSTRUCTORS/RECORD FIELDS");
};