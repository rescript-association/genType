/* Adapted from https://github.com/LexiFi/dead_code_analyzer */

open DeadCommon;

let rec exprNoSideEffects = (expr: Typedtree.expression) =>
  switch (expr.exp_desc) {
  | Texp_ident(_)
  | Texp_constant(_) => true
  | Texp_construct(_, _, el) => el |> List.for_all(exprNoSideEffects)
  | Texp_function(_) => true
  | Texp_apply(_) => false
  | Texp_sequence(e1, e2) =>
    e1 |> exprNoSideEffects && e2 |> exprNoSideEffects
  | Texp_let(_, vbs, e) =>
    vbs
    |> List.for_all((vb: Typedtree.value_binding) =>
         vb.vb_expr |> exprNoSideEffects
       )
    && e
    |> exprNoSideEffects
  | Texp_record({fields, extended_expression}) =>
    fields
    |> Array.for_all(fieldNoSideEffects)
    && extended_expression
    |> exprOptNoSideEffects
  | Texp_assert(_) => false
  | Texp_match(e, casesOK, casesExn, partial) =>
    partial == Total
    && e
    |> exprNoSideEffects
    && casesOK
    |> List.for_all(caseNoSideEffects)
    && casesExn
    |> List.for_all(caseNoSideEffects)
  | Texp_letmodule(_) => false
  | Texp_lazy(e) => e |> exprNoSideEffects
  | Texp_try(e, cases) =>
    e |> exprNoSideEffects && cases |> List.for_all(caseNoSideEffects)
  | Texp_tuple(el) => el |> List.for_all(exprNoSideEffects)
  | Texp_variant(_lbl, eo) => eo |> exprOptNoSideEffects
  | Texp_field(e, _lid, _ld) => e |> exprNoSideEffects
  | Texp_setfield(_) => false
  | Texp_array(el) => el |> List.for_all(exprNoSideEffects)
  | Texp_ifthenelse(e1, e2, eo) =>
    e1
    |> exprNoSideEffects
    && e2
    |> exprNoSideEffects
    && eo
    |> exprOptNoSideEffects
  | Texp_while(e1, e2) => e1 |> exprNoSideEffects && e2 |> exprNoSideEffects
  | Texp_for(_id, _pat, e1, e2, _dir, e3) =>
    e1
    |> exprNoSideEffects
    && e2
    |> exprNoSideEffects
    && e3
    |> exprNoSideEffects
  | Texp_send(_) => false
  | Texp_new(_) => true
  | Texp_instvar(_) => true
  | Texp_setinstvar(_) => false
  | Texp_override(_) => false
  | Texp_letexception(_ec, e) => e |> exprNoSideEffects
  | Texp_object(_) => true
  | Texp_pack(_) => false
  | Texp_unreachable => false
  | Texp_extension_constructor(_) => true
  }
and exprOptNoSideEffects = eo =>
  switch (eo) {
  | None => true
  | Some(e) => e |> exprNoSideEffects
  }
and fieldNoSideEffects =
    ((_ld, rld): (_, Typedtree.record_label_definition)) =>
  switch (rld) {
  | Kept(_typeExpr) => true
  | Overridden(_lid, e) => e |> exprNoSideEffects
  }
and caseNoSideEffects = ({c_guard, c_rhs}: Typedtree.case) => {
  c_guard |> exprOptNoSideEffects && c_rhs |> exprNoSideEffects;
};

let checkAnyBindingWithNoSideEffects =
    (
      {vb_pat: {pat_desc}, vb_expr: expr, vb_loc: loc}: Typedtree.value_binding,
    ) =>
  switch (pat_desc) {
  | Tpat_any when exprNoSideEffects(expr) && !loc.loc_ghost =>
    let name = "_";
    let path = currentModulePath^ @ [currentModuleName^];
    addDeclaration(~declKind=Value, ~path, ~loc, ~name);
  | _ => ()
  };

let collectValueBinding = (super, self, vb: Typedtree.value_binding) => {
  let oldCurrentBindings = currentBindings^;
  let oldLastBinding = lastBinding^;
  checkAnyBindingWithNoSideEffects(vb);
  let pos =
    switch (vb.vb_pat.pat_desc) {
    | Tpat_var(id, {loc: {loc_start, loc_ghost} as loc})
        when !loc_ghost && !vb.vb_loc.loc_ghost =>
      let name = "+" ++ Ident.name(id);
      let exists =
        switch (PosHash.find_opt(decls, loc_start)) {
        | Some({declKind: Value}) => true
        | Some(_) => assert(false)
        | None => false
        };
      let path = currentModulePath^ @ [currentModuleName^];
      if (!exists) {
        addDeclaration(~declKind=Value, ~path, ~loc, ~name);
      };
      switch (PosHash.find_opt(decls, loc_start)) {
      | None => ()
      | Some(decl) =>
        // Value bindings contain the correct location for the entire declaration: update final position.
        // The previous value was taken from the signature, which only has positions for the id.

        let posAnnotation =
          annotateAtEnd(~pos=loc_start)
            ? vb.vb_loc.loc_end : vb.vb_loc.loc_start;

        PosHash.replace(decls, loc_start, {...decl, posAnnotation});
      };
      loc_start;
    | Tpat_any => getLastBinding()
    | _ when !vb.vb_loc.loc_ghost => vb.vb_loc.loc_start
    | _ => getLastBinding()
    };
  currentBindings := PosSet.add(pos, currentBindings^);
  lastBinding := pos;
  let r = super.Tast_mapper.value_binding(self, vb);
  currentBindings := oldCurrentBindings;
  lastBinding := oldLastBinding;
  r;
};

let collectExpr = (super, self, e: Typedtree.expression) => {
  let posUsage = e.exp_loc.loc_start;
  switch (e.exp_desc) {
  | Texp_ident(
      _path,
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
      _,
      {lbl_loc: {Location.loc_start: posDeclaration, loc_ghost: false}, _},
    )
  | Texp_construct(
      _,
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

  let expr = (self, e) => e |> collectExpr(super, self);
  let pat = (self, p) => p |> collectPattern(super, self);
  let value_binding = (self, vb) => vb |> collectValueBinding(super, self);
  let type_declaration = (self, typeDeclaration: Typedtree.type_declaration) => {
    DeadType.processTypeDeclaration(typeDeclaration);
    super.type_declaration(self, typeDeclaration);
  };
  let structure_item = (self, structureItem: Typedtree.structure_item) => {
    let oldModulePath = currentModulePath^;
    switch (structureItem.str_desc) {
    | Tstr_module({mb_name}) =>
      currentModulePath := [mb_name.txt, ...currentModulePath^]
    | _ => ()
    };
    let result = super.structure_item(self, structureItem);
    currentModulePath := oldModulePath;
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
let processValueDependency =
    (
      (
        {val_loc: {loc_start: {pos_fname: fn1} as pos1, loc_ghost: ghost1}}: Types.value_description,
        {val_loc: {loc_start: {pos_fname: fn2} as pos2, loc_ghost: ghost2}}: Types.value_description,
      ),
    ) => {
  let isInterface = fn =>
    !isImplementation(fn) || !Sys.file_exists(fn ++ "i");
  if (!ghost1 && !ghost2 && pos1 != pos2) {
    let addFileReference = fileIsImplementationOf(fn1, fn2);
    if (!addFileReference) {
      valueReferences
      |> PosHash.mergeSet(~isType=false, ~from=pos2, ~to_=pos1);
    };
    if (addFileReference || isInterface(fn1) && isInterface(fn2)) {
      addValueReference(~addFileReference, pos1, pos2);
    };
  };
};

let processTypeDependency = ((from: Lexing.position, to_: Lexing.position)) => {
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
    (~cmt_value_dependencies, structure: Typedtree.structure) => {
  structure
  |> collectValueReferences.structure(collectValueReferences)
  |> ignore;

  let valueDependencies = cmt_value_dependencies |> List.rev;

  valueDependencies |> List.iter(processValueDependency);

  DeadType.typeDependencies^ |> List.iter(processTypeDependency);

  DeadType.typeDependencies := [];
};