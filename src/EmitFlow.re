open GenFlowCommon;

let genericsString = genericStrings =>
  genericStrings === [] ?
    "" : "<" ++ String.concat(",", genericStrings) ++ ">";

let rec toString = (~exact, typ) =>
  switch (typ) {
  | Optional(typ) => "?" ++ (typ |> toString(~exact))
  | Ident(identPath, typeArguments) =>
    identPath ++ genericsString(List.map(toString(~exact), typeArguments))
  | ObjectType(fields) => fields |> renderObjType(~exact)
  | Arrow(typeParams, valParams, retType) =>
    renderFunType(~exact, typeParams, valParams, retType)
  }
and renderField = (~exact, (lbl, optness, typ)) => {
  let optMarker = optness === NonMandatory ? "?" : "";
  lbl ++ optMarker ++ ":" ++ (typ |> toString(~exact));
}
and renderObjType = (~exact, fields) =>
  (exact ? "{|" : "{")
  ++ String.concat(
       exact ? ", " : "; ",
       List.map(renderField(~exact), fields),
     )
  ++ (exact ? "|}" : "}")
/* TODO: Always drop the final unit argument. */
and renderFunType = (~exact, typeParams, valParams, retType) =>
  genericsString(List.map(toString(~exact), typeParams))
  ++ "("
  ++ String.concat(
       ", ",
       List.map(t => "_:" ++ (t |> toString(~exact)), valParams),
     )
  ++ ") => "
  ++ (retType |> toString(~exact));