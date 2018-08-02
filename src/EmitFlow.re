open GenFlowCommon;

let genericsString = genericStrings =>
  genericStrings === [] ?
    "" : "<" ++ String.concat(",", genericStrings) ++ ">";

let rec toString = typ =>
  switch (typ) {
  | Optional(typ) => "?" ++ toString(typ)
  | Ident(identPath, typeArguments) =>
    identPath ++ genericsString(List.map(toString, typeArguments))
  | ObjectType(fields) => renderObjType(fields)
  | Arrow(typeParams, valParams, retType) =>
    renderFunType(typeParams, valParams, retType)
  }
and renderField = ((lbl, optness, typ)) => {
  let optMarker = optness === NonMandatory ? "?" : "";
  lbl ++ optMarker ++ ":" ++ toString(typ);
}
and renderObjType = fields =>
  "{|" ++ String.concat(", ", List.map(renderField, fields)) ++ "|}"
/* TODO: Always drop the final unit argument. */
and renderFunType = (typeParams, valParams, retType) =>
  genericsString(List.map(toString, typeParams))
  ++ "("
  ++ String.concat(", ", List.map(toString, valParams))
  ++ ") => "
  ++ toString(retType);