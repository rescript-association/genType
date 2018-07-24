open GenFlowCommon;
open EmitAstUtil;

module Convert = {
  /**
 * A pair of functions:
 * - One function that can transform an expression representing JavaScript
 * runtime value into an expression that converts that runtime value into the
 * appropriate Reason runtime value.
 * - The other, performing the opposite AST transformation.
 *
 *     let boolConverter = {
 *       toJS: (exprOfTypeJSBool) => exprThatTransformsThatExprIntoReasonBool,
 *       toReason: (exprOfTypeReasonBool) => exprThatTransformsThatExprIntoJSBool
 *     };
 *     let optionalConverter = {
 *       toJS: (exprOfTypeJSNullableX) => exprThatTransformsThatExprIntoReasonOptional,
 *       toReason: (exprOfTypeReasonOptional) => exprThatTransformsThatExprIntoJSNullable
 *     };
 *
 * So when automatically creating bindings for the Reason function:
 *
 *     let isTrue = (b : option(bool)) : bool =>
 *       switch(b) {
 *       | None => false
 *       | Some(b_) => b_ === true
 *       };
 *
 * You would expose a wrapper of that function to Flow of the form:
 *     let isTrue' = (a) =>
 *       <boolConverter.toJS>(isTrue(
 *         <optionalConverter.toReason>(
 *           <boolConverter.toReason>(a)
 *         )
 *       ))
 *
 * Where the <> regions represent AST-processing substitutions.
 */
  type expressionConverter = {
    toJS: ReasonAst.Parsetree.expression => ReasonAst.Parsetree.expression,
    toReason: ReasonAst.Parsetree.expression => ReasonAst.Parsetree.expression,
  };

  /**
 * Converts Reason values of the following form:
 *
 *     (~x: number, ~y: number) => x + y
 *
 * Into JS functions of the following form:
 *
 *     (jsObj) => thatReasonFunction(/*~x=*/jsObj.x, /*~y=*/jsObj.y)
 *
 * Because we start with Reason as the source of truth for libraries, we have
 * all the type information for callbacks.
 *
 * Use case:
 * ---------
 *
 * Module.re
 *
 *     [@genFlow]
 *     let utility = (~x, ~y) => x + y;
 *
 * Consumer.js
 *
 *     const result = Module.utility({x:0, y:100});
 */
  let convertFunToJS = (origFun, groupedArgConverters, retConverter) => {
    let rec createFun = (revAppArgsSoFar, groupedArgConverters) =>
      switch (groupedArgConverters) {
      | [] =>
        let return =
          mkExprApplyFunLabels(origFun, List.rev(revAppArgsSoFar));
        retConverter.toJS(return);
      | [NamedArgs.Arg(conv), ...tl] =>
        let newIdent = GenIdent.argIdent();
        let nextApp = (
          ReasonAst.Asttypes.Nolabel,
          conv.toReason(mkExprIdentifier(newIdent)),
        );
        mkExprFun(newIdent, createFun([nextApp, ...revAppArgsSoFar], tl));
      | [NamedArgs(nameds), ...tl] =>
        let newIdent = GenIdent.argIdent();
        let mapToAppArg = ((name, optness, conv)) => {
          let ident = mkExprIdentifier(newIdent);
          switch (optness) {
          | Mandatory => (
              ReasonAst.Asttypes.Labelled(name),
              conv.toReason(mkJSGet(ident, name)),
            )
          | NonMandatory => (
              ReasonAst.Asttypes.Optional(name),
              conv.toReason(mkJSGet(ident, name)),
            )
          };
        };
        let newRevArgs = List.rev_map(mapToAppArg, nameds);
        let nextRevAppArgs = List.append(newRevArgs, revAppArgsSoFar);
        mkExprFun(newIdent, createFun(nextRevAppArgs, tl));
      };
    createFun([], groupedArgConverters);
  };

  /**
 * Converts JS values such as the following:
 *
 *     (jsObj) => jsObj.x + jsObj.y
 *    /      \
 *    --------
 *   Where we assume that a grouping of Reason arguments has a corresponding
 *   JavaScript object containing the arguments keyed by their named argument
 *   labels.
 *
 * Into Reason functions of the following form:
 *
 *     (~x: number, ~y: number) => (calls_the_underlying_js_function)
 *
 * Because we start with Reason as the source of truth for libraries, we have
 * all the type information for callbacks.
 *
 * Use case:
 * ---------
 *
 * Module.re
 *
 *     [@genFlow]
 *     let consumeCallback = cb => cb(~y=10, ());
 *
 * Consumer.js
 *
 *     const result = Module.consumeCallback(jsObj => jsObj.x + jsObj.y);
 *
 * TODO: - Create Tests Cases With Mismatched Arity
 * TODO:   - Possibly inject runtime .length checks/adapters.
 * TODO: - Map optional types to optional fields.
 * TODO: - Make any final unit args implicit (just as it is right now with BS
 *         FFI for [LOWPRI].
 */
  let convertFunToReason = (origFun, groupedArgConverters, retConverter) => {
    let rec createFun = (revAppArgsSoFar, groupedArgConverters) =>
      switch (groupedArgConverters) {
      | [] =>
        let return =
          mkExprApplyFunLabels(origFun, List.rev(revAppArgsSoFar));
        retConverter.toReason(return);
      | [NamedArgs.Arg(c), ...tl] =>
        let newIdent = GenIdent.argIdent();
        let nextApp = (
          ReasonAst.Asttypes.Nolabel,
          c.toJS(mkExprIdentifier(newIdent)),
        );
        mkExprFun(newIdent, createFun([nextApp, ...revAppArgsSoFar], tl));
      /* We know these several named args will be represented by a single object.
       * The group has a "base" new identifier, and we append the label onto that
       * to determine individual bound pattern identifiers for each argument.
       * This won't work with named arguments that use the same name repeatedly,
       * but that also won't map well to object keys, so that's okay. We need to
       * add a test case for that. */
      | [NamedArgs(nameds), ...tl] =>
        let identBase = GenIdent.argIdent();
        let mapToJSObjRow = ((nextName, optness, c)) => (
          nextName,
          c.toJS(mkExprIdentifier(identBase ++ nextName)),
        );
        let jsObj = (
          ReasonAst.Asttypes.Nolabel,
          mkJSObj(List.map(mapToJSObjRow, nameds)),
        );
        let revAppArgsWObject = [jsObj, ...revAppArgsSoFar];
        List.fold_right(
          ((nextName, _nextOptness, _), soFar) =>
            mkExprFun(
              ~label=ReasonAst.Asttypes.Labelled(nextName),
              identBase ++ nextName,
              soFar,
            ),
          nameds,
          createFun(revAppArgsWObject, tl),
        );
      };
    createFun([], groupedArgConverters);
  };

  let fn = (labeledConverters, retConverter) => {
    let revGroupedArgConverters =
      NamedArgs.groupReversed([], [], labeledConverters);
    let groupedArgConverters = NamedArgs.reverse(revGroupedArgConverters);
    {
      toReason: expr =>
        convertFunToReason(expr, groupedArgConverters, retConverter),
      toJS: expr => convertFunToJS(expr, groupedArgConverters, retConverter),
    };
  };

  /**
   * Utility for constructing new optional converters.
   * The only conversion needed is from ?typ to option(typ)
   * since ?typ admits null, while option(typ) only admits undefined.
   */
  let optionalConverter = expressionConverter => {
    toReason: expr => {
      /* Need to create let binding to avoid double side effects from substitution! */
      let origJSExprIdent = GenIdent.jsMaybeIdent();
      let convertedReasonIdent = GenIdent.optIdent();
      mkExprLet(
        origJSExprIdent,
        expr,
        mkExprIf(
          mkTripleEqualExpr(
            mkExprIdentifier(origJSExprIdent),
            mkJSRawExpr("null"),
          ),
          noneExpr,
          mkExprLet(
            convertedReasonIdent,
            expressionConverter.toReason(mkExprIdentifier(origJSExprIdent)),
            mkExpr(
              mkExprConstructorDesc(
                ~payload=mkExprIdentifier(convertedReasonIdent),
                "Some",
              ),
            ),
          ),
        ),
      );
    },
    toJS: expr => expressionConverter.toJS(expr),
  };

  let identity = {toReason: expr => expr, toJS: expr => expr};
  let option = optionalConverter;
  let optionalArgument = converter => converter;
  let unit: expressionConverter = {
    toReason: expr => expr,
    toJS: expr => expr,
  };

  let rec apply = x =>
    switch (x) {
    | CodeItem.Unit => unit
    | Identity => identity
    | OptionalArgument(y) => optionalArgument(y |> apply)
    | Option(y) => option(y |> apply)
    | Fn((y, z)) =>
      fn(y |> List.map(((label, x)) => (label, x |> apply)), z |> apply)
    };
};

let emitStructureItem = structureItem => {
  let outputFormatter = Format.str_formatter;
  Reason_toolchain.RE.print_implementation_with_comments(
    outputFormatter,
    ([structureItem], []),
  );
  Format.pp_print_flush(outputFormatter, ());
  Format.flush_str_formatter();
};

let emitCodeItem = codeItem =>
  switch (codeItem) {
  | CodeItem.RawJS(s) => "Js_unsafe.raw_stmt(\n  \"" ++ s ++ "\",\n);\n"

  | FlowTypeBinding(id, flowType) =>
    let mkFlowTypeBinding = (name, flowType) =>
      mkBinding(
        mkPatternIdent(
          BuckleScriptPostProcessLib.Patterns.flowTypeAnnotationPrefix ++ name,
        ),
        mkExpr(
          ReasonAst.Parsetree.Pexp_constant(
            Pconst_string(Flow.render(flowType), None),
          ),
        ),
      );
    [mkFlowTypeBinding(id, flowType)]
    |> mkStructItemValBindings
    |> emitStructureItem;

  | ValueBinding(inputModuleName, id, converter) =>
    let consumeProp = mkExprIdentifier(inputModuleName ++ "." ++ id);
    mkStructItemValBindings([
      mkBinding(
        mkPatternIdent(id),
        (converter |> Convert.apply).toJS(consumeProp),
      ),
    ])
    |> emitStructureItem;

  | ConstructorBinding(
      (annotationBindingName, constructorFlowType),
      constructorAlias,
      convertableFlowTypes,
      modulePath,
      leafName,
      _runtimeValue,
    ) =>
    let createVariantFunction = (convertableFlowTypes, modPath, leafName) => {
      let rec buildUp =
              (
                argLen,
                revConvertedArgs,
                convertableFlowTypes,
                modPath,
                leafName,
              ) =>
        switch (revConvertedArgs, convertableFlowTypes) {
        | ([], []) => mkExprConstructorDesc(modPath ++ "." ++ leafName)
        | ([hd, ...tl], []) =>
          mkExprConstructorDesc(
            ~payload=mkTuple(List.rev(revConvertedArgs)),
            modPath ++ "." ++ leafName,
          )
        | (_, [(converter, _), ...tl]) =>
          /* TODO: Apply the converter if available. */
          let maker = tl === [] ? mkExprExplicitArity : mkExpr;
          let name = GenIdent.argIdent();
          let argExpr =
            (converter |> Convert.apply).toReason(mkExprIdentifier(name));
          mkExprFunDesc(
            name,
            maker(
              buildUp(
                argLen + 1,
                [argExpr, ...revConvertedArgs],
                tl,
                modPath,
                leafName,
              ),
            ),
          );
        };
      buildUp(0, [], List.rev(convertableFlowTypes), modPath, leafName);
    };
    (
      mkFlowAnnotationStructItem(annotationBindingName, constructorFlowType)
      |> emitStructureItem
    )
    ++ (
      mkStructItemValBindings([
        mkBinding(
          mkPattern(Ppat_var(located(constructorAlias))),
          mkExpr(
            createVariantFunction(convertableFlowTypes, modulePath, leafName),
          ),
        ),
      ])
      |> emitStructureItem
    );

  | ComponentBinding(
      inputModuleName,
      flowPropGenerics,
      id,
      converter,
      _propsTypeName,
    ) =>
    let makeIdentifier =
      mkExprIdentifier(inputModuleName ++ "." ++ Ident.name(id));
    let jsPropsIdent = mkExprIdentifier("jsProps");
    let getChildrenFromJSProps = mkJSGet(jsPropsIdent, "children");
    let jsPropsToReason =
      mkExprFun(
        "jsProps",
        mkExprApplyFun(
          (converter |> Convert.apply).toJS(makeIdentifier),
          flowPropGenerics == None ?
            [jsPropsIdent] : [jsPropsIdent, getChildrenFromJSProps],
        ),
      );
    mkStructItemValBindings([
      mkBinding(
        mkPatternIdent("component"),
        mkExprApplyFunLabels(
          mkExprIdentifier("ReasonReact.wrapReasonForJs"),
          [
            (
              ReasonAst.Asttypes.Labelled("component"),
              mkExprIdentifier(inputModuleName ++ "." ++ "component"),
            ),
            (ReasonAst.Asttypes.Nolabel, jsPropsToReason),
          ],
        ),
      ),
    ])
    |> emitStructureItem;
  };

let emitCodeItems = codeItems =>
  List.map(emitCodeItem, codeItems) |> String.concat("");