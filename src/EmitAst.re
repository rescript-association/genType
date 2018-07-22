open GenFlowCommon;
open EmitAstUtil;
let structureItem = structureItem => {
  let outputFormatter = Format.str_formatter;
  Reason_toolchain.RE.print_implementation_with_comments(
    outputFormatter,
    ([structureItem], []),
  );
  Format.pp_print_flush(outputFormatter, ());
  Format.flush_str_formatter();
};

let mkFlowTypeBinding = (name, flowType) =>
  EmitAstUtil.(
    mkBinding(
      mkPatternIdent(
        BuckleScriptPostProcessLib.Patterns.flowTypeAnnotationPrefix ++ name,
      ),
      mkExpr(
        ReasonAst.Parsetree.Pexp_constant(
          Pconst_string(Flow.render(flowType), None),
        ),
      ),
    )
  );

let createVariantFunction = (convertableFlowTypes, modPath, leafName) => {
  let rec buildUp =
          (argLen, revConvertedArgs, convertableFlowTypes, modPath, leafName) =>
    switch (revConvertedArgs, convertableFlowTypes) {
    | ([], []) =>
      EmitAstUtil.mkExprConstructorDesc(modPath ++ "." ++ leafName)
    | ([hd, ...tl], []) =>
      EmitAstUtil.mkExprConstructorDesc(
        ~payload=EmitAstUtil.mkTuple(List.rev(revConvertedArgs)),
        modPath ++ "." ++ leafName,
      )
    | (_, [(converter, _), ...tl]) =>
      /* TODO: Apply the converter if available. */
      let maker = EmitAstUtil.(tl === [] ? mkExprExplicitArity : mkExpr);
      let name = GenIdent.argIdent();
      let argExpr =
        (converter |> CodeItem.Convert.apply).toReason(
          EmitAstUtil.mkExprIdentifier(name),
        );
      EmitAstUtil.mkExprFunDesc(
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

let codeItem = codeItem =>
  switch (codeItem) {
  | CodeItem.RawJS(s) => "Js_unsafe.raw_stmt(\n  \"" ++ s ++ "\",\n);\n"
  | FlowTypeBinding(id, flowType) =>
    [mkFlowTypeBinding(id, flowType)]
    |> mkStructItemValBindings
    |> structureItem
  | FlowAnnotation(annotationBindingName, constructorFlowType) =>
    mkFlowAnnotationStructItem(annotationBindingName, constructorFlowType)
    |> structureItem
  | ValueBinding(inputModuleName, id, converter) =>
    let consumeProp =
      mkExprIdentifier(inputModuleName ++ "." ++ Ident.name(id));
    mkStructItemValBindings([
      mkBinding(
        mkPatternIdent(Ident.name(id)),
        (converter |> CodeItem.Convert.apply).toJS(consumeProp),
      ),
    ])
    |> structureItem;
  | ConstructorBinding(
      constructorAlias,
      convertableFlowTypes,
      modulePath,
      leafName,
    ) =>
    mkStructItemValBindings([
      mkBinding(
        mkPattern(Ppat_var(located(constructorAlias))),
        mkExpr(
          createVariantFunction(convertableFlowTypes, modulePath, leafName),
        ),
      ),
    ])
    |> structureItem
  | ComponentBinding(inputModuleName, flowPropGenerics, id, converter) =>
    let makeIdentifier =
      mkExprIdentifier(inputModuleName ++ "." ++ Ident.name(id));
    let jsPropsIdent = mkExprIdentifier("jsProps");
    let getChildrenFromJSProps = mkJSGet(jsPropsIdent, "children");
    let jsPropsToReason =
      mkExprFun(
        "jsProps",
        mkExprApplyFun(
          (converter |> CodeItem.Convert.apply).toJS(makeIdentifier),
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
    |> structureItem;
  };

let codeItems = codeItems =>
  List.map(codeItem, codeItems) |> String.concat("");