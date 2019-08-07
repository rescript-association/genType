type import = {
  name: string,
  importPath: ImportPath.t,
};

type attributePayload =
  | BoolPayload(bool)
  | FloatPayload(string)
  | IntPayload(int)
  | StringPayload(string)
  | TuplePayload(list(attributePayload))
  | UnrecognizedPayload;

type t =
  | GenType
  | GenTypeOpaque
  | NoGenType;

let toString = annotation =>
  switch (annotation) {
  | GenType => "GenType"
  | GenTypeOpaque => "GenTypeOpaque"
  | NoGenType => "NoGenType"
  };

let tagIsGenType = s => s == "genType" || s == "gentype";
let tagIsGenTypeAs = s => s == "genType.as" || s == "gentype.as";

let tagIsGenTypeImport = s => s == "genType.import" || s == "gentype.import";

let tagIsGenTypeOpaque = s => s == "genType.opaque" || s == "gentype.opaque";

let rec getAttributePayload = (checkText, attributes: Typedtree.attributes) => {
  let rec fromExpr = (expr: Parsetree.expression) =>
    switch (expr) {
    | {pexp_desc: Pexp_constant(Const_string(s, _)), _} =>
      Some(StringPayload(s))
    | {pexp_desc: Pexp_constant(Const_int(n)), _} => Some(IntPayload(n))
    | {pexp_desc: Pexp_constant(Const_float(s)), _} =>
      Some(FloatPayload(s))
    | {
        pexp_desc: Pexp_construct({txt: Lident(("true" | "false") as s)}, _),
        _,
      } =>
      Some(BoolPayload(s == "true"))
    | {pexp_desc: Pexp_tuple(exprs), _} =>
      let payloads =
        exprs
        |> List.rev
        |> List.fold_left(
             (payloads, expr) =>
               switch (expr |> fromExpr) {
               | Some(payload) => [payload, ...payloads]
               | None => payloads
               },
             [],
           );
      Some(TuplePayload(payloads));
    | _ => None
    };
  switch (attributes) {
  | [] => None
  | [({Asttypes.txt, _}, payload), ..._tl] when checkText(txt) =>
    switch (payload) {
    | PStr([]) => Some(UnrecognizedPayload)
    | PStr([{pstr_desc: Pstr_eval(expr, _), _}, ..._]) => expr |> fromExpr

    | PStr([{pstr_desc: Pstr_extension(_), _}, ..._]) =>
      Some(UnrecognizedPayload)
    | PStr([{pstr_desc: Pstr_value(_), _}, ..._]) =>
      Some(UnrecognizedPayload)
    | PStr([{pstr_desc: Pstr_primitive(_), _}, ..._]) =>
      Some(UnrecognizedPayload)
    | PStr([{pstr_desc: Pstr_type(_), _}, ..._]) =>
      Some(UnrecognizedPayload)
    | PStr([{pstr_desc: Pstr_typext(_), _}, ..._]) =>
      Some(UnrecognizedPayload)
    | PStr([{pstr_desc: Pstr_exception(_), _}, ..._]) =>
      Some(UnrecognizedPayload)
    | PStr([{pstr_desc: Pstr_module(_), _}, ..._]) =>
      Some(UnrecognizedPayload)
    | PStr([{pstr_desc: Pstr_recmodule(_), _}, ..._]) =>
      Some(UnrecognizedPayload)
    | PStr([{pstr_desc: Pstr_modtype(_), _}, ..._]) =>
      Some(UnrecognizedPayload)
    | PStr([{pstr_desc: Pstr_open(_), _}, ..._]) =>
      Some(UnrecognizedPayload)
    | PStr([{pstr_desc: Pstr_class(_), _}, ..._]) =>
      Some(UnrecognizedPayload)
    | PStr([{pstr_desc: Pstr_class_type(_), _}, ..._]) =>
      Some(UnrecognizedPayload)
    | PStr([{pstr_desc: Pstr_include(_), _}, ..._]) =>
      Some(UnrecognizedPayload)
    | PStr([{pstr_desc: Pstr_attribute(_), _}, ..._]) =>
      Some(UnrecognizedPayload)
    | PTyp(_) => Some(UnrecognizedPayload)
    | PPat(_) => Some(UnrecognizedPayload)
    }
  | [_hd, ...tl] => getAttributePayload(checkText, tl)
  };
};

let getAttributeRenaming = attributes =>
  switch (attributes |> getAttributePayload(tagIsGenTypeAs)) {
  | Some(StringPayload(s)) => Some(s)
  | None =>
    switch (attributes |> getAttributePayload(tagIsGenType)) {
    | Some(StringPayload(s)) => Some(s)
    | _ => None
    }
  | _ => None
  };

let getAttributeImportRenaming = attributes => {
  let attributeImport = attributes |> getAttributePayload(tagIsGenTypeImport);
  let attributeRenaming = attributes |> getAttributeRenaming;
  switch (attributeImport, attributeRenaming) {
  | (Some(StringPayload(importString)), _) => (
      Some(importString),
      attributeRenaming,
    )
  | (
      Some(
        TuplePayload([
          StringPayload(importString),
          StringPayload(renameString),
        ]),
      ),
      _,
    ) => (
      Some(importString),
      Some(renameString),
    )
  | _ => (None, attributeRenaming)
  };
};

let hasAttribute = (checkText, attributes: Typedtree.attributes) =>
  getAttributePayload(checkText, attributes) != None;

let fromAttributes = (attributes: Typedtree.attributes) =>
  if (hasAttribute(tagIsGenType, attributes)) {
    GenType;
  } else if (hasAttribute(tagIsGenTypeOpaque, attributes)) {
    GenTypeOpaque;
  } else {
    NoGenType;
  };

let hasGenTypeAnnotation = attributes =>
  [GenType, GenTypeOpaque]
  |> List.mem(fromAttributes(attributes))
  || attributes
  |> getAttributePayload(tagIsGenTypeImport) != None;

let rec moduleTypeHasGenTypeAnnotation =
        ({mty_desc, _}: Typedtree.module_type) =>
  switch (mty_desc) {
  | Tmty_signature(signature) => signature |> signatureHasGenTypeAnnotation
  | Tmty_ident(_)
  | Tmty_functor(_)
  | Tmty_with(_)
  | Tmty_typeof(_)
  | Tmty_alias(_) => false
  }
and moduleDeclarationHasGenTypeAnnotation =
    ({md_attributes, md_type, _}: Typedtree.module_declaration) =>
  md_attributes
  |> hasGenTypeAnnotation
  || md_type
  |> moduleTypeHasGenTypeAnnotation
and signatureItemHasGenTypeAnnotation =
    (signatureItem: Typedtree.signature_item) =>
  switch (signatureItem) {
  | {Typedtree.sig_desc: Typedtree.Tsig_type(typeDeclarations), _} =>
    typeDeclarations
    |> List.exists(dec => dec.Typedtree.typ_attributes |> hasGenTypeAnnotation)
  | {Typedtree.sig_desc: Tsig_value(valueDescription), _} =>
    valueDescription.val_attributes |> hasGenTypeAnnotation
  | {Typedtree.sig_desc: Typedtree.Tsig_module(moduleDeclaration), _} =>
    moduleDeclaration |> moduleDeclarationHasGenTypeAnnotation
  | _ => false
  }
and signatureHasGenTypeAnnotation = (signature: Typedtree.signature) =>
  signature.Typedtree.sig_items
  |> List.exists(signatureItemHasGenTypeAnnotation);

let rec structureItemHasGenTypeAnnotation =
        (structureItem: Typedtree.structure_item) =>
  switch (structureItem) {
  | {str_desc: Typedtree.Tstr_type(typeDeclarations), _} =>
    typeDeclarations
    |> List.exists(dec => dec.Typedtree.typ_attributes |> hasGenTypeAnnotation)
  | {str_desc: Tstr_value(_loc, valueBindings), _} =>
    valueBindings
    |> List.exists(vb => vb.Typedtree.vb_attributes |> hasGenTypeAnnotation)
  | {str_desc: Tstr_primitive(valueDescription), _} =>
    valueDescription.val_attributes |> hasGenTypeAnnotation
  | {str_desc: Tstr_module(moduleBinding), _} =>
    moduleBinding |> moduleBindingHasGenTypeAnnotation
  | {str_desc: Tstr_recmodule(moduleBindings), _} =>
    moduleBindings |> List.exists(moduleBindingHasGenTypeAnnotation)
  | {str_desc: Tstr_include({incl_attributes, incl_mod}), _} =>
    incl_attributes
    |> hasGenTypeAnnotation
    || incl_mod
    |> moduleExprHasGenTypeAnnotation
  | _ => false
  }
and moduleExprHasGenTypeAnnotation = (moduleExpr: Typedtree.module_expr) =>
  switch (moduleExpr.mod_desc) {
  | Tmod_structure(structure)
  | Tmod_constraint({mod_desc: Tmod_structure(structure)}, _, _, _) =>
    structure |> structureHasGenTypeAnnotation
  | Tmod_constraint(_)
  | Tmod_ident(_)
  | Tmod_functor(_)
  | Tmod_apply(_)
  | Tmod_unpack(_) => false
  }
and moduleBindingHasGenTypeAnnotation =
    ({mb_expr, mb_attributes, _}: Typedtree.module_binding) =>
  mb_attributes
  |> hasGenTypeAnnotation
  || mb_expr
  |> moduleExprHasGenTypeAnnotation
and structureHasGenTypeAnnotation = (structure: Typedtree.structure) =>
  structure.str_items |> List.exists(structureItemHasGenTypeAnnotation);

let sanitizeVariableName = name =>
  name |> Str.global_replace(Str.regexp("-"), "_");

let importFromString = importString: import => {
  let name = {
    let base = importString |> Filename.basename;
    (
      try (base |> Filename.chop_extension) {
      | Invalid_argument(_) => base
      }
    )
    |> sanitizeVariableName;
  };
  let importPath = ImportPath.fromStringUnsafe(importString);
  {name, importPath};
};