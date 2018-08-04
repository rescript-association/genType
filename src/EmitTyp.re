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