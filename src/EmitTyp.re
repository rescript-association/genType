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
  ++ String.concat(", ", List.map(toString(~exact), valParams))
  ++ ") => "
  ++ (retType |> toString(~exact));

let any = Ident("any", []);
let toString = (~config) => toString(~exact=true);
let commentBeforeRequire = (~config) => "";

let emitExportType = (~config, ~opaque, ~typeName, ~typeParams, typ) =>
  "export"
  ++ (opaque ? " opaque " : " ")
  ++ "type "
  ++ typeName
  ++ typeParams
  ++ " = "
  ++ typ
  ++ (opaque ? " // Reason type already checked. Making it opaque" : "");

let requireReact = (~config) => true;

let reactComponentType = (~config) => "React$ComponentType";

let fileHeader = (~config) => "/* @flow strict */\n";

let componentExportName = (~config, ~moduleName) => "component";

let outputFileSuffix = (~config) => ".re.js";

let generatedModuleExtension = (~config) => ".re";

let importTypeAs = (~config, ~typeName, ~asTypeName, ~importPath) =>
  "import type {"
  ++ typeName
  ++ (
    switch (asTypeName) {
    | Some(asT) => " as " ++ asT
    | None => ""
    }
  )
  ++ "} from '"
  ++ (importPath |> ImportPath.toString)
  ++ "'";

let blockTagValue = (~config, i) => string_of_int(i);