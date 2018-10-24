open GenTypeCommon;

let translateValueBinding =
    (
      ~config,
      ~outputFileRelative,
      ~resolver,
      ~moduleItemGen,
      ~fileName,
      ~typeEnv,
      valueBinding,
    )
    : Translation.t => {
  let {Typedtree.vb_pat, vb_attributes, vb_expr, _} = valueBinding;
  let moduleItem = moduleItemGen |> Runtime.newModuleItem;
  typeEnv |> TypeEnv.updateModuleItem(~moduleItem);
  let typeExpr = vb_expr.exp_type;
  switch (vb_pat.pat_desc, Annotation.fromAttributes(vb_attributes)) {
  | (Tpat_var(id, _), GenType) when Ident.name(id) == "make" =>
    id
    |> Ident.name
    |> Translation.translateComponent(
         ~config,
         ~outputFileRelative,
         ~resolver,
         ~fileName,
         ~typeEnv,
         ~typeExpr,
       )
  | (Tpat_var(id, _), GenType) =>
    id
    |> Ident.name
    |> Translation.translateValue(
         ~config,
         ~outputFileRelative,
         ~resolver,
         ~fileName,
         ~typeEnv,
         ~typeExpr,
       )
  | _ => Translation.empty
  };
};

let rec translateStructureItem =
        (
          ~config,
          ~outputFileRelative,
          ~resolver,
          ~moduleItemGen,
          ~fileName,
          ~typeEnv,
          structItem,
        )
        : Translation.t =>
  switch (structItem) {
  | {Typedtree.str_desc: Typedtree.Tstr_type(typeDeclarations), _} => {
      importTypes: [],
      codeItems: [],
      typeDeclarations:
        typeDeclarations
        |> TranslateTypeDeclarations.translateTypeDeclarations(
             ~config,
             ~outputFileRelative,
             ~resolver,
             ~typeEnv,
           ),
    }

  | {Typedtree.str_desc: Tstr_value(_loc, valueBindings), _} =>
    valueBindings
    |> List.map(
         translateValueBinding(
           ~config,
           ~outputFileRelative,
           ~resolver,
           ~moduleItemGen,
           ~fileName,
           ~typeEnv,
         ),
       )
    |> Translation.combine

  | {Typedtree.str_desc: Tstr_primitive(valueDescription), _} =>
    /* external declaration */
    valueDescription
    |> Translation.translatePrimitive(
         ~config,
         ~outputFileRelative,
         ~resolver,
         ~fileName,
         ~typeEnv,
       )

  | {Typedtree.str_desc: Tstr_module(moduleBinding), _} =>
    moduleBinding
    |> translateModuleBinding(
         ~config,
         ~outputFileRelative,
         ~resolver,
         ~fileName,
         ~typeEnv,
         ~moduleItemGen,
       )
  | {Typedtree.str_desc: Tstr_recmodule(moduleBindings), _} =>
    moduleBindings
    |> List.map(
         translateModuleBinding(
           ~config,
           ~outputFileRelative,
           ~resolver,
           ~fileName,
           ~typeEnv,
           ~moduleItemGen,
         ),
       )
    |> Translation.combine

  | _ => Translation.empty
  }
and translateStructure =
    (~config, ~outputFileRelative, ~resolver, ~fileName, ~typeEnv, structure)
    : list(Translation.t) => {
  let moduleItemGen = Runtime.moduleItemGen();
  structure.Typedtree.str_items
  |> List.map(structItem =>
       structItem
       |> translateStructureItem(
            ~config,
            ~outputFileRelative,
            ~resolver,
            ~moduleItemGen,
            ~fileName,
            ~typeEnv,
          )
     );
}
and translateModuleBinding =
    (
      ~config,
      ~outputFileRelative,
      ~resolver,
      ~fileName,
      ~typeEnv,
      ~moduleItemGen,
      {mb_id, mb_expr, mb_attributes, _}: Typedtree.module_binding,
    )
    : Translation.t => {
  let name = mb_id |> Ident.name;
  switch (mb_expr.mod_desc) {
  | Tmod_structure(structure) =>
    let _isAnnotated = mb_attributes |> Annotation.hasGenTypeAnnotation;
    let moduleItem = moduleItemGen |> Runtime.newModuleItem;
    typeEnv |> TypeEnv.updateModuleItem(~moduleItem);
    structure
    |> translateStructure(
         ~config,
         ~outputFileRelative,
         ~resolver,
         ~fileName,
         ~typeEnv=typeEnv |> TypeEnv.newModule(~name),
       )
    |> Translation.combine;

  | Tmod_ident(_) =>
    logNotImplemented("Tmod_ident");
    Translation.empty;
  | Tmod_functor(_) =>
    logNotImplemented("Tmod_functor");
    Translation.empty;
  | Tmod_apply(_) =>
    logNotImplemented("Tmod_apply");
    Translation.empty;
  | Tmod_constraint(_) =>
    logNotImplemented("Tmod_constraint");
    Translation.empty;
  | Tmod_unpack(_) =>
    logNotImplemented("Tmod_unpack");
    Translation.empty;
  };
};