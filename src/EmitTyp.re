open GenFlowCommon;

let any = Ident("any", []);
let genericsString = EmitFlow.genericsString;
let toString = EmitFlow.toString(~exact=true);
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

let requireReact = true;