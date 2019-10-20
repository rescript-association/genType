/* Adapted from https://github.com/LexiFi/dead_code_analyzer */

open DeadCommon;

let rec getSignature = (~isfunc=false, moduleType: Types.module_type) =>
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

let rec collectExport = (~path, ~moduleName, si: Types.signature_item) =>
  switch (si) {
  | Sig_value(id, {Types.val_loc, val_kind}) when !val_loc.Location.loc_ghost =>
    let isPrimitive =
      switch (val_kind) {
      | Val_prim(_) => true
      | _ => false
      };
    if (!isPrimitive || analyzeExternals) {
      export(~path, ~moduleName, ~decs=valueDecs, ~id, ~loc=val_loc);
    };
  | Sig_type(id, t, _) =>
    if (analyzeTypes) {
      DeadType.collectExport([id, ...path], moduleName, t);
    }
  | (
      Sig_module(id, {Types.md_type: moduleType, _}, _) |
      Sig_modtype(id, {Types.mtd_type: Some(moduleType), _})
    ) as s =>
    let collect =
      switch (s) {
      | Sig_modtype(_) => false
      | _ => true
      };
    if (collect) {
      getSignature(moduleType)
      |> List.iter(collectExport(~path=[id, ...path], ~moduleName));
    };
  | _ => ()
  };

let collectValueBinding = (super, self, vb: Typedtree.value_binding) => {
  let oldPos = currentBindingPos^;
  let pos =
    switch (vb.vb_pat.pat_desc) {
    | Tpat_var(id, loc) when !loc.loc.loc_ghost => loc.loc.loc_start
    | _ when !vb.vb_loc.loc_ghost => vb.vb_loc.loc_start
    | _ => currentBindingPos^
    };
  currentBindingPos := pos;
  let r = super.Tast_mapper.value_binding(self, vb);
  currentBindingPos := oldPos;
  r;
};

let collectExpr = (super, self, e: Typedtree.expression) => {
  let posUsage = e.exp_loc.loc_start;
  open Ident;
  switch (e.exp_desc) {
  | Texp_ident(
      path,
      _,
      {
        Types.val_loc: {
          Location.loc_start: posDeclaration,
          loc_ghost: false,
          _,
        },
        _,
      },
    ) =>
    addValueReference(~addFileReference=true, posDeclaration, posUsage)

  | Texp_field(
      _,
      x,
      {
        lbl_loc: {Location.loc_start: posDeclaration, loc_ghost: false, _},
        _,
      },
    )
  | Texp_construct(
      x,
      {
        cstr_loc: {Location.loc_start: posDeclaration, loc_ghost: false, _},
        _,
      },
      _,
    ) =>
    if (analyzeTypes) {
      DeadType.collectReferences(~posDeclaration, ~posUsage);
    }

  | _ => ()
  };
  super.Tast_mapper.expr(self, e);
};

/* Traverse the AST */
let collectReferences = {
  /* Tast_mapper */
  let super = Tast_mapper.default;
  let wrap = (f, ~getPos, ~self, x) => {
    let last = lastPos^;
    let thisPos = getPos(x);
    if (thisPos != Lexing.dummy_pos) {
      lastPos := thisPos;
    };
    let r = f(super, self, x);
    lastPos := last;
    r;
  };

  let expr = (self, e) =>
    e |> wrap(collectExpr, ~getPos=x => x.exp_loc.loc_start, ~self);
  let value_binding = (self, vb) =>
    vb
    |> wrap(
         collectValueBinding,
         ~getPos=x => x.vb_expr.exp_loc.loc_start,
         ~self,
       );
  Tast_mapper.{...super, expr, value_binding};
};

let isImplementation = fn => fn.[String.length(fn) - 1] != 'i';

/* Merge a location's references to another one's */
let processValueDependency = ((vd1, vd2)) => {
  let pos1 = vd1.Types.val_loc.loc_start
  and pos2 = vd2.Types.val_loc.loc_start;
  let fn1 = pos1.pos_fname
  and fn2 = pos2.pos_fname;
  let isInterface = fn =>
    !isImplementation(fn) || !Sys.file_exists(fn ++ "i");

  if (fn1 != none_ && fn2 != none_ && pos1 != pos2) {
    PosHash.mergeSet(valueReferences, pos1, pos2);
    if (isInterface(fn1) && isInterface(fn2)) {
      let addFileReference =
        !(isImplementation(fn1) && isImplementation(fn2));
      addValueReference(~addFileReference, pos1, pos2);
    };
  };
};

let processSignature = (fn, signature: Types.signature) => {
  let moduleName = getModuleName(fn);
  let module_id = Ident.create(String.capitalize_ascii(moduleName));
  signature
  |> List.iter(sig_item =>
       collectExport(~path=[module_id], ~moduleName, sig_item)
     );
  lastPos := Lexing.dummy_pos;
};

let processStructure =
    (~cmtiExists, cmt_value_dependencies, structure: Typedtree.structure) => {
  structure
  |> collectReferences.Tast_mapper.structure(collectReferences)
  |> ignore;

  let valueDependencies = cmt_value_dependencies |> List.rev;

  valueDependencies |> List.iter(processValueDependency);
  if (cmtiExists) {
    let clean = pos => {
      let fn = pos.Lexing.pos_fname;
      if (isImplementation(fn)
          && getModuleName(fn) == getModuleName(currentSrc^)) {
        if (verbose) {
          GenTypeCommon.logItem(
            "clean %s\n",
            pos |> posToString(~printCol=true, ~shortFile=true),
          );
        };

        PosHash.remove(valueReferences, pos);
      };
    };
    valueDependencies
    |> List.iter(((vd1, vd2)) => {
         clean(vd1.Types.val_loc.loc_start);
         clean(vd2.Types.val_loc.loc_start);
       });
  };
};