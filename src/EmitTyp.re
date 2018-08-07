open GenFlowCommon;

let genericsString = genericStrings =>
  genericStrings === [] ?
    "" : "<" ++ String.concat(",", genericStrings) ++ ">";

let rec toString = (~config, ~exact, typ) =>
  switch (typ) {
  | Optional(typ) => "?" ++ (typ |> toString(~config, ~exact))
  | Ident(identPath, typeArguments) =>
    identPath
    ++ genericsString(List.map(toString(~config, ~exact), typeArguments))
  | ObjectType(fields) => fields |> renderObjType(~config, ~exact)
  | Arrow(typeParams, valParams, retType) =>
    renderFunType(~config, ~exact, typeParams, valParams, retType)
  }
and renderField = (~config, ~exact, (lbl, optness, typ)) => {
  let optMarker = optness === NonMandatory ? "?" : "";
  lbl ++ optMarker ++ ":" ++ (typ |> toString(~config, ~exact));
}
and renderObjType = (~config, ~exact, fields) =>
  (exact ? "{|" : "{")
  ++ String.concat(
       exact ? ", " : "; ",
       List.map(renderField(~config, ~exact), fields),
     )
  ++ (exact ? "|}" : "}")
/* TODO: Always drop the final unit argument. */
and renderFunType = (~config, ~exact, typeParams, valParams, retType) =>
  genericsString(List.map(toString(~config, ~exact), typeParams))
  ++ "("
  ++ String.concat(
       ", ",
       List.mapi(
         (i, t) => {
           let parameterName =
             config.language != "typescript" ?
               "" : "_" ++ string_of_int(i + 1) ++ ":";
           parameterName ++ (t |> toString(~config, ~exact));
         },
         valParams,
       ),
     )
  ++ ") => "
  ++ (retType |> toString(~config, ~exact));

let any = Ident("any", []);
let toString = (~config) =>
  toString(~config, ~exact=config.language != "typescript");
let commentBeforeRequire = (~config) =>
  config.language != "typescript" ?
    "" : "// tslint:disable-next-line:no-var-requires\n";

let emitExportType = (~config, ~opaque, ~typeName, ~typeParams, typ) =>
  if (config.language != "typescript") {
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

let requireReact = (~config) => config.language != "typescript";

let reactComponentType = (~config) =>
  config.language != "typescript" ?
    "React$ComponentType" : "React.ComponentClass";

let fileHeader = (~config) =>
  config.language != "typescript" ?
    "/* @flow strict */\n" : "/* Typescript file generated */";

let componentExportName = (~config, ~moduleName) =>
  config.language != "typescript" ?
    "component" : ModuleName.toString(moduleName);

let outputFileSuffix = (~config) =>
  config.language != "typescript" ? ".re.js" : ".tsx";

let generatedModuleExtension = (~config) =>
  config.language != "typescript" ? ".re" : "";

let importTypeAs = (~config, ~typeName, ~asTypeName, ~importPath) =>
  (config.language != "typescript" ? "" : "\n")
  ++ "import "
  ++ (config.language != "typescript" ? "type " : "")
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

let blockTagValue = (~config, i) =>
  string_of_int(i) ++ (config.language != "typescript" ? "" : " as any");

let shimExtension = (~config) =>
  config.language != "typescript" ? ".shim.js" : ".shim.ts";

let imporReact = (~config) =>
  config.language != "typescript" ? "" : "import * as React from \"react\";";