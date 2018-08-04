open GenFlowCommon;

let any = Ident("any", []);
let genericsString = EmitFlow.genericsString;
let toString = EmitFlow.toString;
let commentBeforeRequire = "";

let emitExportType = (~opaque, ~typeName, ~typeParams, typ) =>
  "export"
  ++ (opaque ? " opaque " : " ")
  ++ "type "
  ++ typeName
  ++ typeParams
  ++ " = "
  ++ typ
  ++ (opaque ? " // Reason type already checked. Making it opaque" : "");