open GenTypeCommon;

type exportType = {
  nameAs: option(string),
  opaque: option(bool),
  optTyp: option(typ),
  typeVars: list(string),
  resolvedTypeName: string,
};

type constructorTyp = {
  typeVars: list(string),
  argTypes: list(typ),
  variant,
};

type exportVariantLeaf = {
  exportType,
  constructorTyp,
  argTypes: list(typ),
  leafName: string,
  recordValue: Runtime.recordValue,
};

type exportVariantType = {
  leaves: list(exportVariantLeaf),
  resolvedTypeName: string,
  typeVars: list(string),
  variants: list(variant),
};

type importComponent = {
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

type exportKind =
  | ExportType(exportType)
  | ExportVariantType(exportVariantType);

type exportFromTypeDeclaration = {
  exportKind,
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