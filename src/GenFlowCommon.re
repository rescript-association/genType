/***
 * Copyright 2004-present Facebook. All Rights Reserved.
 */

module StringMap = Map.Make(String);

let suffix = ".re.js";

let resolveSourceModule = moduleName =>
  /* TODO: find the path from the module name */
  Filename.(concat(parent_dir_name, moduleName ++ ".bs"));

let resolveGeneratedModule = moduleName =>
  /* TODO: find the path from the module name */
  Filename.(concat(current_dir_name, moduleName ++ ".re"));

/**
 * Returns the generated JS module name for a given Reason module name.
 */
let jsModuleNameForReasonModuleName = (modulesMap, reasonModuleName) => {
  let tentative = reasonModuleName ++ ".bs";
  StringMap.mem(tentative, modulesMap) ?
    StringMap.find(tentative, modulesMap) :
    resolveGeneratedModule(reasonModuleName);
};
let tagSearch = "genFlow";
let tagSearchOpaque = "genFlow.opaque";
let componentTagSearch = tagSearch;
let jsTypeNameForAnonymousTypeID = id => "T" ++ string_of_int(id);
let jsTypeNameForTypeParameterName = s => String.capitalize;

type optionalness =
  | NonMandatory
  | Mandatory;

module Flow = {
  /* Introduction of type variables (for all) */
  type typ =
    | Optional(typ)
    /* List of typ is the type arguments applied */
    | Ident(string, list(typ))
    | ObjectType(list((string, optionalness, typ)))
    /* List of typ is the type parameters abstracted. Not the arguments
     * applied. */
    | Arrow(list(typ), list(typ), typ);
  let genericsString = genericStrings =>
    genericStrings === [] ?
      "" : "<" ++ String.concat(",", genericStrings) ++ ">";
  let rec render = typ =>
    switch (typ) {
    | Optional(typ) => "?" ++ render(typ)
    | Ident(identPath, typeArguments) =>
      identPath ++ genericsString(List.map(render, typeArguments))
    | ObjectType(fields) => renderObjType(fields)
    | Arrow(typeParams, valParams, retType) =>
      renderFunType(typeParams, valParams, retType)
    }
  and renderField = ((lbl, optness, typ)) => {
    let optMarker = optness === NonMandatory ? "?" : "";
    lbl ++ optMarker ++ ":" ++ render(typ);
  }
  and renderObjType = fields =>
    "{|" ++ String.concat(", ", List.map(renderField, fields)) ++ "|}"
  /* TODO: Always drop the final unit argument. */
  and renderFunType = (typeParams, valParams, retType) =>
    genericsString(List.map(render, typeParams))
    ++ "("
    ++ String.concat(", ", List.map(render, valParams))
    ++ ") => "
    ++ render(retType);
  let applyTypeArgs = (typ, args) =>
    switch (typ) {
    | Optional(typ) => typ
    | Ident(identPath, _) => Ident(identPath, args)
    | ObjectType(fields) => typ
    | Arrow(typeParams, valParams, retType) => typ
    };
  /* Applies type parameters to types (for all) */
  let abstractTheTypeParameters = (typ, params) =>
    switch (typ) {
    | Optional(_) => typ
    | Ident(_) => typ
    | ObjectType(_) => typ
    | Arrow(_, valParams, retType) => Arrow(params, valParams, retType)
    };
  /* Assumes post processing will inject an alias to any. */
  let anyAlias = Ident("any", []);
};

/* Generate fresh identifiers */
module GenIdent = {
  /*
   * Keep a few banks of identifiers to make them more readable.
   */

  let identCount = {contents: 0};

  let argIdentCount = {contents: 0};

  let jsMaybeIdentCount = {contents: 0};

  let optIdentCount = {contents: 0};

  let propsTypeNameCount = {contents: 0};

  let resetPerStructure = () => {
    identCount.contents = 0;
    argIdentCount.contents = 0;
    jsMaybeIdentCount.contents = 0;
    optIdentCount.contents = 0;
  };

  let resetPerFile = () => {
    resetPerStructure();
    propsTypeNameCount.contents = 0;
  };

  let ident_ = count =>
    count < 26 ?
      String.make(1, Char.chr(97 /*a*/ + count)) :
      "var" ++ string_of_int(count);

  let ident = () => {
    let ret = ident_(identCount.contents);
    identCount.contents = identCount.contents + 1;
    ret;
  };

  let argIdent = () => {
    let ret = "arg" ++ String.capitalize(ident_(argIdentCount.contents));
    argIdentCount.contents = argIdentCount.contents + 1;
    ret;
  };

  let jsMaybeIdent = () => {
    let ret =
      "jsMaybe" ++ String.capitalize(ident_(jsMaybeIdentCount.contents));
    jsMaybeIdentCount.contents = jsMaybeIdentCount.contents + 1;
    ret;
  };

  let optIdent = () => {
    let ret = "optData" ++ String.capitalize(ident_(optIdentCount.contents));
    optIdentCount.contents = optIdentCount.contents + 1;
    ret;
  };

  let propsTypeName = () => {
    propsTypeNameCount.contents = propsTypeNameCount.contents + 1;
    "Props"
    ++ (
      propsTypeNameCount.contents == 1 ?
        "" : string_of_int(propsTypeNameCount.contents)
    );
  };
};