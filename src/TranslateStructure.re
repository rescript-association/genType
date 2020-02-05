open GenTypeCommon;

let rec addAnnotationsToTypes =
        (~config, ~expr: Typedtree.expression, types: list(type_)) =>
  switch (expr.exp_desc, types) {
  | (_, [GroupOfLabeledArgs(fields), ...nextTypes]) =>
    let (fields1, nextTypes1) =
      addAnnotationsToFields(~config, expr, fields, nextTypes);
    [GroupOfLabeledArgs(fields1), ...nextTypes1];
  | (Texp_function({cases: [{c_rhs}]}), [type_, ...nextTypes]) =>
    let nextTypes1 = nextTypes |> addAnnotationsToTypes(~config, ~expr=c_rhs);
    [type_, ...nextTypes1];
  | _ => types
  }
and addAnnotationsToFields =
    (~config, expr: Typedtree.expression, fields: fields, types: list(type_)) =>
  switch (expr.exp_desc, fields, types) {
  | (_, [], _) => ([], types |> addAnnotationsToTypes(~config, ~expr))
  | (Texp_function({cases: [{c_rhs}]}), [field, ...nextFields], _) =>
    let (nextFields1, types1) =
      addAnnotationsToFields(~config, c_rhs, nextFields, types);
    let (nameJS, nameRE) =
      TranslateTypeDeclarations.renameRecordField(
        ~attributes=expr.exp_attributes,
        ~config,
        ~nameRE=field.nameRE,
      );
    ([{...field, nameJS, nameRE}, ...nextFields1], types1);

  | _ => (fields, types)
  };

/* Recover from expr the renaming annotations on named arguments. */
let addAnnotationsToFunctionType =
    (~config, expr: Typedtree.expression, type_: type_) =>
  switch (type_) {
  | Function(function_) =>
    let argTypes =
      function_.argTypes |> addAnnotationsToTypes(~config, ~expr);
    Function({...function_, argTypes});
  | _ => type_
  };

let removeValueBindingDuplicates = structureItems => {
  let rec processBindings = (bindings: list(Typedtree.value_binding), ~seen) =>
    switch (bindings) {
    | [{vb_pat: {pat_desc: Tpat_var(id, _)}} as binding, ...otherBindings] =>
      let name = Ident.name(id);
      if (seen^ |> StringSet.mem(name)) {
        otherBindings |> processBindings(~seen);
      } else {
        seen := seen^ |> StringSet.add(name);
        [binding, ...otherBindings |> processBindings(~seen)];
      };
    | [binding, ...otherBindings] => [
        binding,
        ...otherBindings |> processBindings(~seen),
      ]
    | [] => []
    };
  let rec processItems = (items: list(Typedtree.structure_item), ~acc, ~seen) =>
    switch (items) {
    | [
        {Typedtree.str_desc: Tstr_value(loc, valueBindings)} as item,
        ...otherItems,
      ] =>
      let bindings = valueBindings |> processBindings(~seen);
      let item = {...item, str_desc: Tstr_value(loc, bindings)};
      otherItems |> processItems(~acc=[item, ...acc], ~seen);
    | [item, ...otherItems] =>
      otherItems |> processItems(~acc=[item, ...acc], ~seen)
    | [] => acc
    };
  structureItems
  |> List.rev
  |> processItems(~acc=[], ~seen=ref(StringSet.empty));
};

let translateValueBinding =
    (
      ~config,
      ~outputFileRelative,
      ~resolver,
      ~moduleItemGen,
      ~typeEnv,
      {Typedtree.vb_pat, vb_attributes, vb_expr},
    )
    : Translation.t => {
  switch (vb_pat.pat_desc) {
  | Tpat_var(id, _) =>
    let name = id |> Ident.name;
    if (Debug.translation^) {
      logItem("Translate Value Binding %s\n", name);
    };
    let moduleItem = moduleItemGen |> Runtime.newModuleItem(~name);
    typeEnv |> TypeEnv.updateModuleItem(~nameOpt=Some(name), ~moduleItem);

    if (Annotation.fromAttributes(vb_attributes) == GenType) {
      id
      |> Ident.name
      |> Translation.(
           Ident.name(id) == "make" ? translateComponent : translateValue
         )(
           ~attributes=vb_attributes,
           ~config,
           ~outputFileRelative,
           ~resolver,
           ~typeEnv,
           ~typeExpr=vb_expr.exp_type,
           ~addAnnotationsToFunction=
             addAnnotationsToFunctionType(~config, vb_expr),
         );
    } else {
      Translation.empty;
    };

  | _ => Translation.empty
  };
};

let rec removeDuplicateValueBindings =
        (structureItems: list(Typedtree.structure_item)) =>
  switch (structureItems) {
  | [
      {Typedtree.str_desc: Tstr_value(loc, valueBindings)} as structureItem,
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
          {mb_id, mb_expr}: Typedtree.module_binding,
        )
        : Translation.t => {
  let name = mb_id |> Ident.name;
  if (Debug.translation^) {
    logItem("Translate Module Binding %s\n", name);
  };
  let moduleItem = moduleItemGen |> Runtime.newModuleItem(~name);
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
  | {Typedtree.str_desc: Typedtree.Tstr_type(_, typeDeclarations)} => {
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

  | {Typedtree.str_desc: Tstr_value(_loc, valueBindings)} =>
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

  | {Typedtree.str_desc: Tstr_primitive(valueDescription)} =>
    /* external declaration */
    valueDescription
    |> Translation.translatePrimitive(
         ~config,
         ~outputFileRelative,
         ~resolver,
         ~typeEnv,
       )

  | {Typedtree.str_desc: Tstr_module(moduleBinding)} =>
    moduleBinding
    |> translateModuleBinding(
         ~config,
         ~outputFileRelative,
         ~resolver,
         ~typeEnv,
         ~moduleItemGen,
       )

  | {Typedtree.str_desc: Tstr_modtype(moduleTypeDeclaration)} =>
    moduleTypeDeclaration
    |> TranslateSignature.translateModuleTypeDeclaration(
         ~config,
         ~outputFileRelative,
         ~resolver,
         ~typeEnv,
       )

  | {Typedtree.str_desc: Tstr_recmodule(moduleBindings)} =>
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

  | {Typedtree.str_desc: Tstr_include({incl_type: signature})} =>
    signature
    |> TranslateSignatureFromTypes.translateSignatureFromTypes(
         ~config,
         ~outputFileRelative,
         ~resolver,
         ~typeEnv,
       )
    |> Translation.combine

  | {Typedtree.str_desc: Tstr_eval(_)} =>
    logNotImplemented("Tstr_eval " ++ __LOC__);
    Translation.empty;
  | {Typedtree.str_desc: Tstr_typext(_)} =>
    logNotImplemented("Tstr_typext " ++ __LOC__);
    Translation.empty;
  | {Typedtree.str_desc: Tstr_exception(_)} =>
    logNotImplemented("Tstr_exception " ++ __LOC__);
    Translation.empty;
  | {Typedtree.str_desc: Tstr_open(_)} =>
    logNotImplemented("Tstr_open " ++ __LOC__);
    Translation.empty;
  | {Typedtree.str_desc: Tstr_class(_)} =>
    logNotImplemented("Tstr_class " ++ __LOC__);
    Translation.empty;
  | {Typedtree.str_desc: Tstr_class_type(_)} =>
    logNotImplemented("Tstr_class_type " ++ __LOC__);
    Translation.empty;
  | {Typedtree.str_desc: Tstr_attribute(_)} =>
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
  |> removeValueBindingDuplicates
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