open GenTypeCommon;

type exportType = {
  opaque: option(bool),
  typeVars: list(string),
  resolvedTypeName: string,
  optTyp: option(typ),
};

type exportVariantType = {
  typeParams: list(typ),
  variants: list(variant),
  name: string,
};

type importComponent = {
  exportType,
  importAnnotation: Annotation.import,
  childrenTyp: typ,
  propsFields: fields,
  propsTypeName: string,
  fileName: ModuleName.t,
};

type importValue = {
  valueName: string,
  importAnnotation: Annotation.import,
  typ,
  fileName: ModuleName.t,
};

type exportComponent = {
  exportType,
  fileName: ModuleName.t,
  moduleName: ModuleName.t,
  propsTypeName: string,
  componentType: typ,
  typ,
};

type exportValue = {
  fileName: ModuleName.t,
  resolvedName: string,
  valueAccessPath: string,
  typ,
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

type exportKind =
  | ExportType(exportType)
  | ExportVariantLeaf(exportVariantLeaf)
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
  strictLocal: bool,
};

type exportTypeItem = {
  typeVars: list(string),
  typ,
  annotation: Annotation.t,
  importTypes: list(importType),
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