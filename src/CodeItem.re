open GenTypeCommon;

type exportType = {
  opaque: option(bool),
  typeVars: list(string),
  resolvedTypeName: string,
  optTyp: option(typ),
  genTypeKind,
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

type wrapModule = {codeItems: list(t)}
and t =
  | ExportComponent(exportComponent)
  | ExportType(exportType)
  | ExportValue(exportValue)
  | ExportVariantLeaf(exportVariantLeaf)
  | ExportVariantType(exportVariantType)
  | ImportComponent(importComponent)
  | ImportValue(importValue);