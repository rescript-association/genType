open GenFlowCommon;

let genericsString = genericStrings =>
  genericStrings === [] ?
    "" : "<" ++ String.concat(",", genericStrings) ++ ">";

let rec toString = (~language, ~exact, typ) =>
  switch (typ) {
  | Optional(typ) => "?" ++ (typ |> toString(~language, ~exact))
  | Ident(identPath, typeArguments) =>
    identPath
    ++ genericsString(List.map(toString(~language, ~exact), typeArguments))
  | ObjectType(fields) => fields |> renderObjType(~language, ~exact)
  | Arrow(typeParams, valParams, retType) =>
    renderFunType(~language, ~exact, typeParams, valParams, retType)
  }
and renderField = (~language, ~exact, (lbl, optness, typ)) => {
  let optMarker = optness === NonMandatory ? "?" : "";
  lbl ++ optMarker ++ ":" ++ (typ |> toString(~language, ~exact));
}
and renderObjType = (~language, ~exact, fields) =>
  (exact ? "{|" : "{")
  ++ String.concat(
       exact ? ", " : "; ",
       List.map(renderField(~language, ~exact), fields),
     )
  ++ (exact ? "|}" : "}")
/* TODO: Always drop the final unit argument. */
and renderFunType = (~language, ~exact, typeParams, valParams, retType) =>
  genericsString(List.map(toString(~language, ~exact), typeParams))
  ++ "("
  ++ String.concat(
       ", ",
       List.mapi(
         (i, t) => {
           let parameterName =
             language == Flow ? "" : "_" ++ string_of_int(i + 1) ++ ":";
           parameterName ++ (t |> toString(~language, ~exact));
         },
         valParams,
       ),
     )
  ++ ") => "
  ++ (retType |> toString(~language, ~exact));

let toString = (~language) => toString(~language, ~exact=language == Flow);
let commentBeforeRequire = (~language) =>
  language == Flow ? "" : "// tslint:disable-next-line:no-var-requires\n";

let emitExportType = (~language, ~opaque, ~typeName, ~typeParams, typ) => {
  let typeParams =
    genericsString(List.map(toString(~language), typeParams));
  if (language == Flow) {
    "export"
    ++ (opaque ? " opaque " : " ")
    ++ "type "
    ++ typeName
    ++ typeParams
    ++ " = "
    ++ typ
    ++ (opaque ? " // Reason type already checked. Making it opaque" : "");
  } else if (opaque) {
    "// tslint:disable-next-line:max-classes-per-file \n"
    ++ "export abstract class "
    ++ typeName
    ++ " { protected opaque:any } /* simulate opaque types */";
  } else {
    "// tslint:disable-next-line:interface-over-type-literal\n"
    ++ "export type "
    ++ typeName
    ++ typeParams
    ++ " = "
    ++ typ;
  };
};

let emitExportUnionType = (~language, ~name, ~typeParams, ~leafTypes) =>
  "export type "
  ++ name
  ++ genericsString(List.map(toString(~language), typeParams))
  ++ " =\n  | "
  ++ String.concat("\n  | ", List.map(toString(~language), leafTypes));

let requireReact = (~language) => language == Flow;

let importReact = (~language) =>
  language == Flow ? "" : "import * as React from \"react\";";

let reactComponentType = (~language) =>
  language == Flow ? "React$ComponentType" : "React.ComponentClass";

let fileHeader = (~language) =>
  language == Flow ?
    "/* @flow strict */\n" : "/* Typescript file generated */";

let componentExportName = (~language, ~moduleName) =>
  language == Flow ? "component" : ModuleName.toString(moduleName);

let outputFileSuffix = (~language) => language == Flow ? ".re.js" : ".tsx";

let generatedModuleExtension = (~language) => language == Flow ? ".re" : "";

let importTypeAs = (~language, ~typeName, ~asTypeName, ~importPath) =>
  (language == Flow ? "" : "\n")
  ++ "import "
  ++ (language == Flow ? "type " : "")
  ++ "{"
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

let blockTagValue = (~language, i) =>
  string_of_int(i) ++ (language == Flow ? "" : " as any");

let shimExtension = (~language) => language == Flow ? ".shim.js" : ".shim.ts";