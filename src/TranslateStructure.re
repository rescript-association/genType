open GenTypeCommon;

let rec addAnnotationsToTyps =
        (~expr: Typedtree.expression, types: list(type_)) =>
  switch (expr.exp_desc, types) {
  | (_, [GroupOfLabeledArgs(fields), ...nextTyps]) =>
    let (fields1, nextTyps1) =
      addAnnotationsToFields(expr, fields, nextTyps);
    [GroupOfLabeledArgs(fields1), ...nextTyps1];
  | (Texp_function(_lbl, [{c_rhs, _}], _), [type_, ...nextTyps]) =>
    let nextTyps1 = nextTyps |> addAnnotationsToTyps(~expr=c_rhs);
    [type_, ...nextTyps1];
  | _ => types
  }
and addAnnotationsToFields =
    (expr: Typedtree.expression, fields: fields, types: list(type_)) =>
  switch (expr.exp_desc, fields, types) {
  | (_, [], _) => ([], types |> addAnnotationsToTyps(~expr))
  | (Texp_function(_lbl, [{c_rhs, _}], _), [field, ...nextFields], _) =>
    switch (expr.exp_attributes |> Annotation.getAttributeRenaming) {
    | Some(s) =>
      let (nextFields1, types1) =
        addAnnotationsToFields(c_rhs, nextFields, types);
      ([{...field, name: s}, ...nextFields1], types1);
    | None =>
      let (nextFields1, types1) =
        addAnnotationsToFields(c_rhs, nextFields, types);
      ([field, ...nextFields1], types1);
    }
  | _ => (fields, types)
  };

/* Because of a bug in 4.02.3, the first attribute of a function type is lost.
   This if fixed in newer versions. */
let bugInOCaml4_02_3 = (expr: Typedtree.expression) =>
  switch (expr.exp_desc) {
  | Texp_function(lbl, [case], pt) => {
      ...expr,
      exp_desc: Texp_function(lbl, [{...case, c_rhs: expr}], pt),
      exp_attributes: [],
    }
  | _ => expr
  };

/* Recover from expr the renaming annotations on named arguments. */
let addAnnotationsToFunctionType = (expr: Typedtree.expression, type_: type_) =>
  switch (type_) {
  | Function(function_) =>
    let argTypes =
      function_.argTypes
      |> addAnnotationsToTyps(~expr=expr |> bugInOCaml4_02_3);
    Function({...function_, argTypes});
  | _ => type_
  };

let translateValueBinding =
    (
      ~config,
      ~outputFileRelative,
      ~resolver,
      ~moduleItemGen,
      ~typeEnv,
      valueBinding,
    )
    : Translation.t => {
  let {Typedtree.vb_pat, vb_attributes, vb_expr, _} = valueBinding;
  let nameOpt =
    switch (vb_pat.pat_desc) {
    | Tpat_var(id, _) => Some(id |> Ident.name)
    | _ => None
    };
  if (Debug.translation^) {
    switch (nameOpt) {
    | Some(s) => logItem("Translate Value Binding %s\n", s)
    | None => ()
    };
  };
  let moduleItem = moduleItemGen |> Runtime.newModuleItem;
  typeEnv |> TypeEnv.updateModuleItem(~nameOpt, ~moduleItem);
  let typeExpr = vb_expr.exp_type;

  switch (vb_pat.pat_desc, Annotation.fromAttributes(vb_attributes)) {
  | (Tpat_var(id, _), GenType) =>
    id
    |> Ident.name
    |> Translation.(
         Ident.name(id) == "make" ? translateComponent : translateValue
       )(
         ~config,
         ~outputFileRelative,
         ~resolver,
         ~typeEnv,
         ~typeExpr,
         ~addAnnotationsToFunction=addAnnotationsToFunctionType(vb_expr),
       )
  | _ => Translation.empty
  };
};

let rec removeDuplicateValueBindings =
        (structureItems: list(Typedtree.structure_item)) =>
  switch (structureItems) {
  | [
      {Typedtree.str_desc: Tstr_value(loc, valueBindings), _} as structureItem,
      ...rest,
    ] =>
    let (boundInRest, filteredRest) = rest |> removeDuplicateValueBindings;
    let valueBindingsFiltered =
      valueBindings
      |> List.filter(valueBinding =>
           switch (valueBinding) {
           | {Typedtree.vb_pat: {pat_desc: Tpat_var(id, _)}} =>
             !(boundInRest |> StringSet.mem(id |> Ident.name))
           | _ => true
           }
         );
    let bound =
      valueBindings
      |> List.fold_left(
           (bound, valueBinding: Typedtree.value_binding) =>
             switch (valueBinding) {
             | {vb_pat: {pat_desc: Tpat_var(id, _)}} =>
               bound |> StringSet.add(id |> Ident.name)
             | _ => bound
             },
           boundInRest,
         );
    (
      bound,
      [
        {...structureItem, str_desc: Tstr_value(loc, valueBindingsFiltered)},
        ...filteredRest,
      ],
    );
  | [structureItem, ...rest] =>
    let (boundInRest, filteredRest) = rest |> removeDuplicateValueBindings;
    (boundInRest, [structureItem, ...filteredRest]);

  | [] => (StringSet.empty, [])
  };

let rec translateModuleBinding =
        (
          ~config,
          ~outputFileRelative,
          ~resolver,
          ~typeEnv,
          ~moduleItemGen,
          {mb_id, mb_expr, _}: Typedtree.module_binding,
        )
        : Translation.t => {
  let name = mb_id |> Ident.name;
  if (Debug.translation^) {
    logItem("Translate Module Binding %s\n", name);
  };
  let moduleItem = moduleItemGen |> Runtime.newModuleItem;
  typeEnv |> TypeEnv.updateModuleItem(~moduleItem);
  let typeEnv = typeEnv |> TypeEnv.newModule(~name);

  switch (mb_expr.mod_desc) {
  | Tmod_structure(structure) =>
    structure
    |> translateStructure(~config, ~outputFileRelative, ~resolver, ~typeEnv)
    |> Translation.combine

  | Tmod_apply(_) =>
    /* Only look at the resulting type of the module */
    switch (mb_expr.mod_type) {
    | Mty_signature(signature) =>
      signature
      |> TranslateSignatureFromTypes.translateSignatureFromTypes(
           ~config,
           ~outputFileRelative,
           ~resolver,
           ~typeEnv,
         )
      |> Translation.combine

    | Mty_ident(_) =>
      logNotImplemented("Mty_ident " ++ __LOC__);
      Translation.empty;
    | Mty_functor(_) =>
      logNotImplemented("Mty_functor " ++ __LOC__);
      Translation.empty;
    | Mty_alias(_) =>
      logNotImplemented("Mty_alias " ++ __LOC__);
      Translation.empty;
    }

  | Tmod_unpack(_, moduleType) =>
    switch (moduleType) {
    | Mty_signature(signature) =>
      signature
      |> TranslateSignatureFromTypes.translateSignatureFromTypes(
           ~config,
           ~outputFileRelative,
           ~resolver,
           ~typeEnv,
         )
      |> Translation.combine

    | Mty_ident(path) =>
      switch (typeEnv |> TypeEnv.lookupModuleTypeSignature(~path)) {
      | None => Translation.empty
      | Some((signature, _)) =>
        signature
        |> TranslateSignature.translateSignature(
             ~config,
             ~outputFileRelative,
             ~resolver,
             ~typeEnv,
           )
        |> Translation.combine
      }

    | Mty_functor(_) =>
      logNotImplemented("Mty_functor " ++ __LOC__);
      Translation.empty;
    | Mty_alias(_) =>
      logNotImplemented("Mty_alias " ++ __LOC__);
      Translation.empty;
    }

  | Tmod_ident(path, _) =>
    let dep = path |> Dependencies.fromPath(~config, ~typeEnv);
    let internal = dep |> Dependencies.isInternal;
    typeEnv |> TypeEnv.addModuleEquation(~dep, ~internal);
    Translation.empty;

  | Tmod_functor(_) =>
    logNotImplemented("Tmod_functor " ++ __LOC__);
    Translation.empty;
  | Tmod_constraint(_, Mty_ident(path), Tmodtype_explicit(_), Tcoerce_none) =>
    switch (typeEnv |> TypeEnv.lookupModuleTypeSignature(~path)) {
    | None => Translation.empty
    | Some((signature, _)) =>
      signature
      |> TranslateSignature.translateSignature(
           ~config,
           ~outputFileRelative,
           ~resolver,
           ~typeEnv,
         )
      |> Translation.combine
    }

  | Tmod_constraint(
      _,
      Mty_signature(signature),
      Tmodtype_explicit(_),
      Tcoerce_none,
    ) =>
    signature
    |> TranslateSignatureFromTypes.translateSignatureFromTypes(
         ~config,
         ~outputFileRelative,
         ~resolver,
         ~typeEnv,
       )
    |> Translation.combine

  | Tmod_constraint(
      {mod_desc: Tmod_structure(structure)},
      _,
      Tmodtype_implicit,
      Tcoerce_structure(_),
    ) =>
    {
      ...structure,
      str_items: structure.str_items |> removeDuplicateValueBindings |> snd,
    }
    |> translateStructure(~config, ~outputFileRelative, ~resolver, ~typeEnv)
    |> Translation.combine

  | Tmod_constraint(_) =>
    logNotImplemented("Tmod_constraint " ++ __LOC__);
    Translation.empty;
  };
}
and translateStructureItem =
    (
      ~config,
      ~outputFileRelative,
      ~resolver,
      ~moduleItemGen,
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
         ~typeEnv,
       )

  | {Typedtree.str_desc: Tstr_module(moduleBinding), _} =>
    moduleBinding
    |> translateModuleBinding(
         ~config,
         ~outputFileRelative,
         ~resolver,
         ~typeEnv,
         ~moduleItemGen,
       )

  | {Typedtree.str_desc: Tstr_modtype(moduleTypeDeclaration), _} =>
    moduleTypeDeclaration
    |> TranslateSignature.translateModuleTypeDeclaration(
         ~config,
         ~outputFileRelative,
         ~resolver,
         ~typeEnv,
       )

  | {Typedtree.str_desc: Tstr_recmodule(moduleBindings), _} =>
    moduleBindings
    |> List.map(
         translateModuleBinding(
           ~config,
           ~outputFileRelative,
           ~resolver,
           ~typeEnv,
           ~moduleItemGen,
         ),
       )
    |> Translation.combine

  | {
      /* Bucklescript's encoding of bs.module: include with constraint. */
      Typedtree.str_desc:
        Tstr_include({
          incl_mod: {
            mod_desc:
              Tmod_constraint(
                {
                  mod_desc:
                    Tmod_structure({
                      str_items: [
                        {str_desc: Tstr_primitive(_)} as structItem1,
                      ],
                    }),
                },
                _,
                _,
                _,
              ),
          },
          _,
        }),
      _,
    } =>
    structItem1
    |> translateStructureItem(
         ~config,
         ~outputFileRelative,
         ~resolver,
         ~moduleItemGen,
         ~typeEnv,
       )

  | {Typedtree.str_desc: Tstr_include({incl_type: signature, _}), _} =>
    signature
    |> TranslateSignatureFromTypes.translateSignatureFromTypes(
         ~config,
         ~outputFileRelative,
         ~resolver,
         ~typeEnv,
       )
    |> Translation.combine

  | {Typedtree.str_desc: Tstr_eval(_), _} =>
    logNotImplemented("Tstr_eval " ++ __LOC__);
    Translation.empty;
  | {Typedtree.str_desc: Tstr_typext(_), _} =>
    logNotImplemented("Tstr_typext " ++ __LOC__);
    Translation.empty;
  | {Typedtree.str_desc: Tstr_exception(_), _} =>
    logNotImplemented("Tstr_exception " ++ __LOC__);
    Translation.empty;
  | {Typedtree.str_desc: Tstr_open(_), _} =>
    logNotImplemented("Tstr_open " ++ __LOC__);
    Translation.empty;
  | {Typedtree.str_desc: Tstr_class(_), _} =>
    logNotImplemented("Tstr_class " ++ __LOC__);
    Translation.empty;
  | {Typedtree.str_desc: Tstr_class_type(_), _} =>
    logNotImplemented("Tstr_class_type " ++ __LOC__);
    Translation.empty;
  | {Typedtree.str_desc: Tstr_attribute(_), _} =>
    logNotImplemented("Tstr_attribute " ++ __LOC__);
    Translation.empty;
  }
and translateStructure =
    (~config, ~outputFileRelative, ~resolver, ~typeEnv, structure)
    : list(Translation.t) => {
  if (Debug.translation^) {
    logItem("Translate Structure\n");
  };
  let moduleItemGen = Runtime.moduleItemGen();
  structure.Typedtree.str_items
  |> List.map(structItem =>
       structItem
       |> translateStructureItem(
            ~config,
            ~outputFileRelative,
            ~resolver,
            ~moduleItemGen,
            ~typeEnv,
          )
     );
};