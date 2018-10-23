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

type importAnnotation = {
  name: string,
  importPath: ImportPath.t,
};

type importComponent = {
  exportType,
  importAnnotation,
  childrenTyp: typ,
  propsFields: fields,
  propsTypeName: string,
  fileName: ModuleName.t,
};

type importValue = {
  valueName: string,
  importAnnotation,
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
  genTypeKind,
};

type t =
  | ExportComponent(exportComponent)
  | ExportFromTypeDeclaration(exportFromTypeDeclaration)
  | ExportValue(exportValue)
  | ImportComponent(importComponent)
  | ImportValue(importValue);