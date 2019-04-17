open GenTypeCommon;

type exportType = {
  nameAs: option(string),
  opaque: option(bool),
  optType: option(type_),
  typeVars: list(string),
  resolvedTypeName: ResolvedName.t,
};

type importComponent = {
  asPath: string,
  childrenTyp: type_,
  exportType,
  importAnnotation: Annotation.import,
  propsFields: fields,
  propsTypeName: string,
};

type importValue = {
  asPath: string,
  importAnnotation: Annotation.import,
  type_,
  valueName: string,
};

type exportComponent = {
  componentAccessPath: string,
  componentType: type_,
  exportType,
  nestedModuleName: option(ModuleName.t),
  propsTypeName: string,
  type_,
  valueAccessPath: string,
};

type exportValue = {
  originalName: string,
  resolvedName: string,
  type_,
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
};

type exportTypeItem = {
  typeVars: list(string),
  type_,
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