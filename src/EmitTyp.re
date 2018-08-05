open GenFlowCommon;

let any = Ident("any", []);
let genericsString = EmitFlow.genericsString;
let toString = (~config) => EmitFlow.toString(~exact=true);
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