/***
 * Copyright 2004-present Facebook. All Rights Reserved.
 */

open Migrate_parsetree;

/* Convert to Reason parse tree version (404) from current */
module From_current = Convert(OCaml_current, OCaml_404);

module ReasonAst = OCaml_404.Ast;

open GenFlowCommon;

/* Utilities to generate an OCaml AST */
let noLexLoc = {
  Lexing.pos_fname: "",
  Lexing.pos_lnum: 0,
  Lexing.pos_cnum: 0,
  Lexing.pos_bol: 0,
};

let noLoc = {
  OCaml_404.Ast.Location.loc_ghost: true,
  loc_start: noLexLoc,
  loc_end: noLexLoc,
};

let located = s => {Location.txt: s, loc: noLoc};

let mkExpr = (~attrs=[], desc) => {
  ReasonAst.Parsetree.pexp_loc: noLoc,
  pexp_desc: desc,
  pexp_attributes: attrs,
};

let mkExprExplicitArity = (~attrs=[], desc) => {
  ReasonAst.Parsetree.pexp_loc: noLoc,
  pexp_desc: desc,
  pexp_attributes: [
    ({Asttypes.txt: "explicit_arity", loc: noLoc}, PStr([])),
    ...attrs,
  ],
};

let mkPattern = desc => {
  ReasonAst.Parsetree.ppat_loc: noLoc,
  ppat_attributes: [],
  ppat_desc: desc,
};

let mkPatternExplicitArity = desc => {
  ReasonAst.Parsetree.ppat_loc: noLoc,
  ppat_attributes: [
    ({Asttypes.txt: "explicit_arity", loc: noLoc}, PStr([])),
  ],
  ppat_desc: desc,
};

let mkBinding = (patDesc, exprDesc) => {
  pvb_loc: noLoc,
  pvb_attributes: [],
  ReasonAst.Parsetree.pvb_pat: patDesc,
  pvb_expr: exprDesc,
};

let mkStructItem = structItemDesc => {
  ReasonAst.Parsetree.pstr_desc: structItemDesc,
  pstr_loc: noLoc,
};

let mkStructItemValBindings = bindings =>
  mkStructItem(Pstr_value(ReasonAst.Asttypes.Nonrecursive, bindings));

let mkStructItemEval = expr =>
  mkStructItem(ReasonAst.Parsetree.Pstr_eval(expr, []));

let mkTuple = expressionList =>
  mkExpr(ReasonAst.Parsetree.Pexp_tuple(expressionList));

let mkExprIdentifier = s =>
  mkExpr(ReasonAst.Parsetree.Pexp_ident(located(Longident.parse(s))));

let mkPatternIdent = s => mkPattern(Ppat_var(located(s)));

let mkCase = (pat, expr) => {
  ReasonAst.Parsetree.pc_lhs: pat,
  pc_guard: None,
  pc_rhs: expr,
};

let mkExprFunDesc = (argName, retExpr) =>
  ReasonAst.Parsetree.Pexp_fun(
    Nolabel,
    None,
    mkPatternIdent(argName),
    retExpr,
  );

let mkExprFun = (~label=ReasonAst.Asttypes.Nolabel, argName, retExpr) =>
  mkExpr(
    ReasonAst.Parsetree.Pexp_fun(
      label,
      None,
      mkPatternIdent(argName),
      retExpr,
    ),
  );

let mkString = s =>
  mkExpr(ReasonAst.Parsetree.Pexp_constant(Pconst_string(s, None)));

/*
 * Since the AST we generate is assumed to be *after* operators are swapped:
 */
let mkTrippleEqualExpr = (expr1, expr2) =>
  mkExpr(
    ReasonAst.Parsetree.Pexp_apply(
      mkExpr(
        ReasonAst.Parsetree.Pexp_ident(located(Longident.parse("=="))),
      ),
      [(Nolabel, expr1), (Nolabel, expr2)],
    ),
  );

let mkJSRawExpr = s =>
  mkExpr(
    ReasonAst.Parsetree.Pexp_apply(
      mkExpr(
        ReasonAst.Parsetree.Pexp_ident(
          located(Longident.parse("Js_unsafe.raw_expr")),
        ),
      ),
      [(Nolabel, mkString(s))],
    ),
  );

let mkJSGet = (obj, fieldName) =>
  mkExpr(
    ReasonAst.Parsetree.Pexp_apply(
      mkExprIdentifier("##"),
      [(Nolabel, obj), (Nolabel, mkExprIdentifier(fieldName))],
    ),
  );

/**
 * Applies a function by name.
 * Pretty heavy handed approach to supressing warnings, but often the function
 * we apply to is an unsafe JS import and the warning about unused argument is
 * too aggressive.
 */
let mkExprApply = (funName, args) =>
  mkExpr(
    ReasonAst.Parsetree.Pexp_apply(
      mkExpr(
        ReasonAst.Parsetree.Pexp_ident(located(Longident.parse(funName))),
      ),
      List.map(argExpr => (ReasonAst.Asttypes.Nolabel, argExpr), args),
    ),
  );

/**
 * Like mkExprApply but accepts a function, not function name.
 */
let mkExprApplyFun = (funExpr, args) =>
  mkExpr(
    ReasonAst.Parsetree.Pexp_apply(
      funExpr,
      List.map(argExpr => (ReasonAst.Asttypes.Nolabel, argExpr), args),
    ),
  );

let mkExprApplyFunLabels = (funExpr, labeledArgs) =>
  mkExpr(ReasonAst.Parsetree.Pexp_apply(funExpr, labeledArgs));

/*
 * "Desc" functions create the actual Variants that go in the records' "_desc"
 * fields. They are the "meat" of the Ast node - without the loc/attributes.
 * These are only really useful when you need to construct forms that have
 * "explicit arity" markers. So basically Constructors().
 */
let mkExprConstructorDesc = (~payload=?, s) =>
  ReasonAst.Parsetree.Pexp_construct(located(Longident.parse(s)), payload);

let mkPatConstructorDesc = (~payload=?, s) =>
  ReasonAst.Parsetree.Ppat_construct(located(Longident.parse(s)), payload);

let noneExpr = mkExpr(mkExprConstructorDesc("None"));

let nonePatDesc = mkPatConstructorDesc("None");

let nonePat = mkPattern(mkPatConstructorDesc("None"));

let rec mkOptionMatch = (argExpr, ifNoneExpr, exprForSomeMatch) => {
  let ident = GenIdent.ident();
  mkExpr(
    ReasonAst.Parsetree.Pexp_match(
      argExpr,
      [
        mkCase(nonePat, ifNoneExpr),
        mkCase(
          mkPatternExplicitArity(
            mkPatConstructorDesc(~payload=mkPatternIdent(ident), "Some"),
          ),
          exprForSomeMatch(ident),
        ),
      ],
    ),
  );
};

let mkExprIf = (exprIf, exprThen, exprElse) =>
  mkExpr(
    ReasonAst.Parsetree.Pexp_ifthenelse(exprIf, exprThen, Some(exprElse)),
  );

let mkExprLet = (name, exprVal, inExpr) =>
  mkExpr(
    ReasonAst.Parsetree.Pexp_let(
      ReasonAst.Asttypes.Nonrecursive,
      [mkBinding(mkPatternIdent(name), exprVal)],
      inExpr,
    ),
  );

let mkExprRecord = rows =>
  mkExpr(
    ReasonAst.Parsetree.Pexp_record(
      List.map(
        ((name, expr)) => (located(Longident.parse(name)), expr),
        rows,
      ),
      None,
    ),
  );

let mkJSObj = rows =>
  mkExpr(
    ReasonAst.Parsetree.Pexp_extension((
      located("bs.obj"),
      PStr([mkStructItemEval(mkExprRecord(rows))]),
    )),
  );

let mkFlowAnnotationStructItem = (bindingName, flowTyp) =>
  mkStructItemValBindings([
    mkBinding(
      mkPattern(Ppat_var(located(bindingName))),
      mkExpr(
        ReasonAst.Parsetree.Pexp_constant(
          Pconst_string(Flow.render(flowTyp), None),
        ),
      ),
    ),
  ]);