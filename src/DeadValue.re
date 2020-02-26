/* Adapted from https://github.com/LexiFi/dead_code_analyzer */

open DeadCommon;

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
      {lbl_loc: {Location.loc_start: posDeclaration, loc_ghost: false}, _},
    )
  | Texp_construct(
      x,
      {cstr_loc: {Location.loc_start: posDeclaration, loc_ghost: false}, _},
      _,
    ) =>
    if (analyzeTypes^) {
      DeadType.addTypeReference(~posDeclaration, ~posUsage);
    }

  | _ => ()
  };
  super.Tast_mapper.expr(self, e);
};

let collectPattern = (super, self, pat: Typedtree.pattern) => {
  let posUsage = pat.pat_loc.loc_start;
  switch (pat.pat_desc) {
  | Tpat_record(cases, _clodsedFlag) =>
    cases
    |> List.iter(
         ((_loc, {Types.lbl_loc: {loc_start: posDeclaration}}, _pat)) =>
         if (analyzeTypes^) {
           DeadType.addTypeReference(~posDeclaration, ~posUsage);
         }
       )
  | _ => ()
  };
  super.Tast_mapper.pat(self, pat);
};

/* Traverse the AST */
let collectValueReferences = {
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
  let pat = (self, p) =>
    p |> wrap(collectPattern, ~getPos=x => x.pat_loc.loc_start, ~self);
  let value_binding = (self, vb) =>
    vb
    |> wrap(
         collectValueBinding,
         ~getPos=x => x.vb_expr.exp_loc.loc_start,
         ~self,
       );
  let type_declaration = (self, typeDeclaration: Typedtree.type_declaration) => {
    DeadType.processTypeDeclaration(typeDeclaration);
    super.type_declaration(self, typeDeclaration);
  };
  let structure_item = (self, structureItem: Typedtree.structure_item) => {
    let oldModulePath = DeadType.modulePath^;
    switch (structureItem.str_desc) {
    | Tstr_module({mb_name}) =>
      DeadType.modulePath := [mb_name.txt, ...DeadType.modulePath^]
    | _ => ()
    };
    let result = super.structure_item(self, structureItem);
    DeadType.modulePath := oldModulePath;
    result;
  };
  Tast_mapper.{
    ...super,
    expr,
    pat,
    structure_item,
    type_declaration,
    value_binding,
  };
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
    valueReferences |> PosHash.mergeSet(~isType=false, ~from=pos2, ~to_=pos1);
    if (isInterface(fn1) && isInterface(fn2)) {
      addValueReference(~addFileReference=false, pos1, pos2);
    };
  };
};

let processTypeDependency = ((to_: Lexing.position, from: Lexing.position)) => {
  let fnTo = to_.pos_fname
  and fnFrom = from.pos_fname;
  let isInterface = fn =>
    !isImplementation(fn) || !Sys.file_exists(fn ++ "i");

  if (fnTo != none_ && fnFrom != none_ && to_ != from) {
    typeReferences |> PosHash.mergeSet(~isType=true, ~from, ~to_);
    if (isInterface(fnTo) && isInterface(fnFrom)) {
      DeadType.addTypeReference(~posDeclaration=to_, ~posUsage=from);
    };
  };
};

let processStructure =
    (~cmtiExists, cmt_value_dependencies, structure: Typedtree.structure) => {
  structure
  |> collectValueReferences.structure(collectValueReferences)
  |> ignore;

  let valueDependencies = cmt_value_dependencies |> List.rev;

  valueDependencies |> List.iter(processValueDependency);

  DeadType.typeDependencies^ |> List.iter(processTypeDependency);

  if (cmtiExists) {
    let clean = (~isType, pos) => {
      let fn = pos.Lexing.pos_fname;
      if (isImplementation(fn) && fn == currentSrc^) {
        if (verbose) {
          Log_.item(
            "%sclean %s\n",
            isType ? "[type] " : "",
            pos |> posToString,
          );
        };

        PosHash.remove(valueReferences, pos);
      };
    };
    valueDependencies
    |> List.iter(((vd1, vd2)) => {
         clean(~isType=false, vd1.Types.val_loc.loc_start);
         clean(~isType=false, vd2.Types.val_loc.loc_start);
       });
    DeadType.typeDependencies^
    |> List.iter(((loc1, loc2)) => {
         clean(~isType=true, loc1);
         clean(~isType=true, loc2);
       });
  };
  DeadType.typeDependencies := [];
};