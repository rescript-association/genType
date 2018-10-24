type import = {
  name: string,
  importPath: ImportPath.t,
};

type attributePayload =
  | UnrecognizedPayload
  | StringPayload(string);

type genTypeKind =
  | Generated
  | GenType
  | GenTypeOpaque
  | NoGenType;

let genTypeKindToString = genTypeKind =>
  switch (genTypeKind) {
  | Generated => "Generated"
  | GenType => "GenType"
  | GenTypeOpaque => "GenTypeOpaque"
  | NoGenType => "NoGenType"
  };

let tagIsGenType = s => s == "genType";
let tagIsGenTypeAs = s => s == "genType" || s == "genType.as";

let tagIsGenTypeImport = s => s == "genType.import";

let tagIsGenTypeOpaque = s => s == "genType.opaque";

let rec getAttributePayload = (checkText, attributes: Typedtree.attributes) =>
  switch (attributes) {
  | [] => None
  | [({Asttypes.txt, _}, payload), ..._tl] when checkText(txt) =>
    switch (payload) {
    | PStr([
        {
          pstr_desc:
            Pstr_eval(
              {pexp_desc: Pexp_constant(Const_string(s, _)), _},
              _,
            ),
          _,
        },
      ]) =>
      Some(StringPayload(s))
    | _ => Some(UnrecognizedPayload)
    }
  | [_hd, ...tl] => getAttributePayload(checkText, tl)
  };

let hasAttribute = (checkText, attributes: Typedtree.attributes) =>
  getAttributePayload(checkText, attributes) != None;

let getGenTypeKind = (attributes: Typedtree.attributes) =>
  if (hasAttribute(tagIsGenType, attributes)) {
    GenType;
  } else if (hasAttribute(tagIsGenTypeOpaque, attributes)) {
    GenTypeOpaque;
  } else {
    NoGenType;
  };

let hasGenTypeAnnotation = attributes =>
  [GenType, GenTypeOpaque]
  |> List.mem(getGenTypeKind(attributes))
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
  | {Typedtree.str_desc: Typedtree.Tstr_type(typeDeclarations), _} =>
    typeDeclarations
    |> List.exists(dec => dec.Typedtree.typ_attributes |> hasGenTypeAnnotation)
  | {Typedtree.str_desc: Tstr_value(_loc, valueBindings), _} =>
    valueBindings
    |> List.exists(vb => vb.Typedtree.vb_attributes |> hasGenTypeAnnotation)
  | {Typedtree.str_desc: Tstr_primitive(valueDescription), _} =>
    valueDescription.val_attributes |> hasGenTypeAnnotation
  | {Typedtree.str_desc: Tstr_module(moduleBinding), _} =>
    moduleBinding |> moduleBindingHasGenTypeAnnotation
  | {Typedtree.str_desc: Tstr_recmodule(moduleBindings), _} =>
    moduleBindings |> List.exists(moduleBindingHasGenTypeAnnotation)
  | _ => false
  }
and moduleBindingHasGenTypeAnnotation =
    ({mb_expr, mb_attributes, _}: Typedtree.module_binding) =>
  mb_attributes
  |> hasGenTypeAnnotation
  || (
    switch (mb_expr.mod_desc) {
    | Tmod_structure(structure) => structure |> structureHasGenTypeAnnotation
    | Tmod_ident(_)
    | Tmod_functor(_)
    | Tmod_apply(_)
    | Tmod_constraint(_)
    | Tmod_unpack(_) => false
    }
  )
and structureHasGenTypeAnnotation = (structure: Typedtree.structure) =>
  structure.str_items |> List.exists(structureItemHasGenTypeAnnotation);

let importFromString = importString: import => {
  let name = {
    let base = importString |> Filename.basename;
    try (base |> Filename.chop_extension) {
    | Invalid_argument(_) => base
    };
  };
  let importPath = ImportPath.fromStringUnsafe(importString);
  {name, importPath};
};