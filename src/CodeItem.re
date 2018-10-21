open GenTypeCommon;

type exportType = {
  opaque: bool,
  typeVars: list(string),
  resolvedTypeName: string,
  optTyp: option(typ),
};

type exportVariantType = {
  typeParams: list(typ),
  variants: list(variant),
  name: string,
};

type importTypeAs = {
  typeName: string,
  asTypeName: option(string),
  importPath: ImportPath.t,
  cmtFile: option(string),
};

type importType =
  | ImportComment(string)
  | ImportTypeAs(importTypeAs);

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
  | ImportType(importType)
  | ImportValue(importValue);

type genTypeKind =
  | NoGenType
  | GenType
  | GenTypeOpaque;

let getPriority = x =>
  switch (x) {
  | ImportComponent(_)
  | ImportType(_)
  | ImportValue(_) => "2low"
  | ExportComponent(_)
  | ExportValue(_)
  | ExportType(_)
  | ExportVariantLeaf(_)
  | ExportVariantType(_) => "1med"
  };

let sortcodeItemsByPriority = codeItems => {
  module M = StringMap;
  let map =
    codeItems
    |> List.fold_left(
         (map, codeItem) => {
           let priority = codeItem |> getPriority;
           let items =
             try (map |> StringMap.find(priority)) {
             | Not_found => []
             };
           map |> StringMap.add(priority, [codeItem, ...items]);
         },
         StringMap.empty,
       );
  let sortedCodeItems = ref([]);
  map
  |> StringMap.iter((_priority, codeItemsAtPriority) =>
       codeItemsAtPriority
       |> List.iter(codeItem =>
            sortedCodeItems := [codeItem, ...sortedCodeItems^]
          )
     );
  sortedCodeItems^;
};

let getImportTypeUniqueName = (importType: importType) =>
  switch (importType) {
  | ImportComment(s) => s
  | ImportTypeAs({typeName, asTypeName, _}) =>
    typeName
    ++ (
      switch (asTypeName) {
      | None => ""
      | Some(s) => "_as_" ++ s
      }
    )
  };

let importTypeCompare = (i1, i2) =>
  compare(i1 |> getImportTypeUniqueName, i2 |> getImportTypeUniqueName);

let toString = (~language, codeItem: t) =>
  switch (codeItem) {
  | ExportComponent({fileName, moduleName, _}) =>
    "ExportComponent fileName:"
    ++ (fileName |> ModuleName.toString)
    ++ " moduleName:"
    ++ (moduleName |> ModuleName.toString)
  | ExportValue({fileName, resolvedName, typ, _}) =>
    "WrapReasonValue"
    ++ " resolvedName:"
    ++ resolvedName
    ++ " moduleName:"
    ++ ModuleName.toString(fileName)
    ++ " typ:"
    ++ EmitTyp.typToString(~language, typ)
  | ExportType({resolvedTypeName, _}) => "ExportType " ++ resolvedTypeName
  | ExportVariantLeaf({leafName, _}) => "WrapVariantLeaf " ++ leafName
  | ExportVariantType({name, _}) => "ExportVariantType " ++ name
  | ImportComponent({importAnnotation, _}) =>
    "ImportComponent " ++ (importAnnotation.importPath |> ImportPath.toString)
  | ImportType(importType) =>
    "ImportType " ++ getImportTypeUniqueName(importType)
  | ImportValue({importAnnotation, _}) =>
    "ImportValue " ++ (importAnnotation.importPath |> ImportPath.toString)
  };