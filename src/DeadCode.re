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

/********   PROCESSING   ********/

let rec collect_export =
        (
          ~mod_type=false,
          path,
          u,
          stock: DeadCommon.decs,
          si: Types.signature_item,
        ) =>
  switch (si) {
  | Sig_value(id, {Types.val_loc})
      when !val_loc.Location.loc_ghost && stock === DeadCommon.decs =>
    DeadCommon.export(path, u, stock, id, val_loc)

  | Sig_type(id, t, _) when stock === DeadCommon.decs =>
    DeadType.collect_export([id, ...path], u, stock, t)

  | (
      Sig_module(id, {Types.md_type: t, _}, _) |
      Sig_modtype(id, {Types.mtd_type: Some(t), _})
    ) as s =>
    let collect =
      switch (s) {
      | Sig_modtype(_) => mod_type
      | _ => true
      };
    if (collect) {
      DeadMod.sign(t)
      |> List.iter(collect_export(~mod_type, [id, ...path], u, stock));
    };
  | _ => ()
  };

/* Keep track of the location of values exported via genType */
module ProcessAnnotations = {
  /* Locations exported to JS */
  let locationsAnnotatedWithGenType = DeadCommon.LocHash.create(1);
  let locationsAnnotatedDead = DeadCommon.LocHash.create(1);

  let isAnnotatedDead = loc =>
    DeadCommon.LocHash.mem(locationsAnnotatedDead, loc);

  let isAnnotatedGentypeOrDead = loc =>
    DeadCommon.LocHash.mem(locationsAnnotatedWithGenType, loc)
    || isAnnotatedDead(loc);

  let locAnnotatedWithGenType = (loc: Lexing.position) => {
    DeadCommon.LocHash.replace(locationsAnnotatedWithGenType, loc, ());
  };

  let locAnnotatedDead = (loc: Lexing.position) => {
    DeadCommon.LocHash.replace(locationsAnnotatedDead, loc, ());
  };

  let processAttributes = (~ignoreInterface, ~loc, attributes) => {
    if (attributes |> Annotation.hasGenTypeAnnotation(~ignoreInterface)) {
      loc |> locAnnotatedWithGenType;
    };
    if (attributes
        |> Annotation.getAttributePayload((==)(DeadCommon.deadAnnotation))
        != None) {
      loc |> locAnnotatedDead;
    };
  };

  let collectExportLocations = (~ignoreInterface) => {
    let super = Tast_mapper.default;
    let value_binding =
        (
          self,
          {vb_attributes, vb_pat} as value_binding: Typedtree.value_binding,
        ) => {
      switch (vb_pat.pat_desc) {
      | Tpat_var(id, pLoc) =>
        vb_attributes
        |> processAttributes(~ignoreInterface, ~loc=pLoc.loc.loc_start)

      | _ => ()
      };
      super.value_binding(self, value_binding);
    };
    let value_description =
        (
          self,
          {val_attributes, val_id, val_loc} as value_description: Typedtree.value_description,
        ) => {
      val_attributes
      |> processAttributes(~ignoreInterface, ~loc=val_loc.loc_start);
      super.value_description(self, value_description);
    };
    {...super, value_binding, value_description};
  };

  let structure = structure => {
    let ignoreInterface = ref(false);
    let collectExportLocations = collectExportLocations(~ignoreInterface);
    structure
    |> collectExportLocations.structure(collectExportLocations)
    |> ignore;
  };
  let signature = signature => {
    let ignoreInterface = ref(false);
    let collectExportLocations = collectExportLocations(~ignoreInterface);
    signature
    |> collectExportLocations.signature(collectExportLocations)
    |> ignore;
  };
};

let currentBindingIsDead = ref(false);
let currentBindingPos = ref(Lexing.dummy_pos);

let collectValueBinding = (super, self, vb: Typedtree.value_binding) => {
  let old = currentBindingIsDead^;
  let oldPos = currentBindingPos^;
  let isAnnotatedDead =
    vb.vb_attributes
    |> Annotation.getAttributePayload((==)(DeadCommon.deadAnnotation))
    != None;
  currentBindingIsDead := isAnnotatedDead;
  currentBindingPos := vb.vb_loc.loc_start;
  let r = super.Tast_mapper.value_binding(self, vb);
  currentBindingIsDead := old;
  currentBindingPos := oldPos;
  r;
};

let addReference = (loc1, loc2) =>
  if (! currentBindingIsDead^) {
    let loc2 =
      !DeadCommon.transitive || currentBindingPos^ == Lexing.dummy_pos
        ? loc2 : currentBindingPos^;
    DeadCommon.LocHash.add_set(DeadCommon.references, loc1, loc2);
  };

let collectExpr = (super, self, e: Typedtree.expression) => {
  let exp_loc = e.exp_loc.loc_start;
  open Ident;
  switch (e.exp_desc) {
  | Texp_ident(
      path,
      _,
      {Types.val_loc: {Location.loc_start: loc, loc_ghost: false, _}, _},
    ) =>
    addReference(loc, exp_loc)

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
    e |> wrap(collectExpr, ~getLoc=x => x.exp_loc, ~self);
  let value_binding = (self, vb) =>
    vb |> wrap(collectValueBinding, ~getLoc=x => x.vb_expr.exp_loc, ~self);
  Tast_mapper.{...super, expr, value_binding};
};

/* Merge a location's references to another one's */
let assoc = ((pos1, pos2)) => {
  let fn1 = pos1.Lexing.pos_fname
  and fn2 = pos2.Lexing.pos_fname;
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
    Hashtbl.mem(DeadCommon.decs, loc)
    || DeadCommon.getModuleName(fn)
    != DeadCommon.getModuleName(DeadCommon.current_src^)
    || !(is_implem(fn) && has_iface(fn));

  if (fn1 != DeadCommon.none_ && fn2 != DeadCommon.none_ && pos1 != pos2) {
    if (fn1 != fn2 && is_implem(fn1) && is_implem(fn2)) {
      DeadCommon.LocHash.merge_set(
        DeadCommon.references,
        pos2,
        DeadCommon.references,
        pos1,
      );
    };
    if (is_iface(fn1, pos1)) {
      DeadCommon.LocHash.merge_set(
        DeadCommon.references,
        pos1,
        DeadCommon.references,
        pos2,
      );
      if (is_iface(fn2, pos2)) {
        addReference(pos1, pos2);
      };
    } else {
      DeadCommon.LocHash.merge_set(
        DeadCommon.references,
        pos2,
        DeadCommon.references,
        pos1,
      );
    };
  };
};

let eom = loc_dep => {
  loc_dep |> List.iter(assoc);
  if (Sys.file_exists(DeadCommon.current_src^ ++ "i")) {
    let clean = loc => {
      let fn = loc.Lexing.pos_fname;
      if (fn.[String.length(fn) - 1] != 'i'
          && DeadCommon.getModuleName(fn)
          == DeadCommon.getModuleName(DeadCommon.current_src^)) {
        DeadCommon.LocHash.remove(DeadCommon.references, loc);
      };
    };
    loc_dep
    |> List.iter(((loc1, loc2)) => {
         clean(loc1);
         clean(loc2);
       });
  };
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

let processStructure =
    (cmt_value_dependencies, structure: Typedtree.structure) => {
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
      ~processAnnotationsSignature,
      ~processAnnotationsStructure,
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
    processAnnotationsSignature(signature);
    process_signature(cmtFilePath, signature.sig_type);
  | Implementation(structure) =>
    let cmtiExists =
      Sys.file_exists((cmtFilePath |> Filename.chop_extension) ++ ".cmti");
    if (!cmtiExists) {
      processAnnotationsStructure(structure);
    };
    processStructure(cmt_value_dependencies, structure);
    if (!cmtiExists) {
      process_signature(cmtFilePath, structure.str_type);
    };
  | _ => ()
  };
};

let report = (~dontReportDead, ~onUnusedValue) => {
  let onItem = (DeadCommon.{pos, path}) => {
    print_string(pos |> DeadCommon.posToString);
    print_string(path);
    print_newline();
  };
  let onUnusedValue = item => {
    onItem(item);
    onUnusedValue(item);
  };
  Printf.printf("\n%s:\n", "UNUSED EXPORTED VALUES");
  DeadCommon.decs |> DeadCommon.report(~dontReportDead, ~onItem=onUnusedValue);
  Printf.printf("\n%s:\n", "UNUSED CONSTRUCTORS/RECORD FIELDS");
  DeadType.decs |> DeadCommon.report(~onItem);
};