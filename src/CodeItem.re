open GenTypeCommon;

type exportType = {
  nameAs: option(string),
  opaque: option(bool),
  optTyp: option(typ),
  typeVars: list(string),
  resolvedTypeName: string,
};

type importComponent = {
  asPath: string,
  childrenTyp: typ,
  exportType,
  fileName: ModuleName.t,
  importAnnotation: Annotation.import,
  propsFields: fields,
  propsTypeName: string,
};

type importValue = {
  asPath: string,
  fileName: ModuleName.t,
  importAnnotation: Annotation.import,
  typ,
  valueName: string,
};

type exportComponent = {
  componentAccessPath: string,
  componentType: typ,
  exportType,
  fileName: ModuleName.t,
  moduleName: ModuleName.t,
  propsTypeName: string,
  typ,
  valueAccessPath: string,
};

type exportValue = {
  fileName: ModuleName.t,
  resolvedName: string,
  typ,
  valueAccessPath: string,
};

type exportFromTypeDeclaration = {
  exportType,
  annotation: Annotation.t,
};

type importType = {
  typeName: string,
  asTypeName: option(string),
  importPath: ImportPath.t,
  cmtFile: option(string),
};

type exportTypeItem = {
  typeVars: list(string),
  typ,
  annotation: Annotation.t,
};

type exportTypeMap = StringMap.t(exportTypeItem);

type typeDeclaration = {
  exportFromTypeDeclaration,
  importTypes: list(importType),
};

type t =
  | ExportComponent(exportComponent)
  | ExportValue(exportValue)
  | ImportComponent(importComponent)
  | ImportValue(importValue);

type translation = {
  importTypes: list(importType),
  codeItems: list(t),
  typeDeclarations: list(typeDeclaration),
};