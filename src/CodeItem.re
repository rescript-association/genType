open GenTypeCommon;

type exportType = {
  opaque: bool,
  typeVars: list(string),
  resolvedTypeName: string,
  comment: option(string),
  typ,
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

type wrapJsComponent = {
  exportType,
  importAnnotation,
  childrenTyp: typ,
  propsFields: fields,
  propsTypeName: string,
  moduleName: ModuleName.t,
};

type wrapJsValue = {
  valueName: string,
  importAnnotation,
  typ,
  moduleName: ModuleName.t,
};

type wrapReasonComponent = {
  exportType,
  moduleName: ModuleName.t,
  propsTypeName: string,
  componentType: typ,
  typ,
};

type wrapReasonValue = {
  moduleName: ModuleName.t,
  resolvedName: string,
  valueAccessPath: string,
  typ,
};

type constructorTyp = {
  typeVars: list(string),
  argTypes: list(typ),
  variant,
};

type wrapVariantLeaf = {
  exportType,
  constructorTyp,
  argTypes: list(typ),
  leafName: string,
  recordValue: Runtime.recordValue,
};

type wrapModule = {codeItems: list(t)}
and t =
  | ExportType(exportType)
  | ExportVariantType(exportVariantType)
  | ImportType(importType)
  | WrapJsComponent(wrapJsComponent)
  | WrapJsValue(wrapJsValue)
  | WrapReasonComponent(wrapReasonComponent)
  | WrapReasonValue(wrapReasonValue)
  | WrapVariantLeaf(wrapVariantLeaf);

type genTypeKind =
  | NoGenType
  | GenType
  | GenTypeOpaque;

let getPriority = x =>
  switch (x) {
  | ImportType(_)
  | WrapJsComponent(_)
  | WrapJsValue(_) => "2low"
  | ExportType(_)
  | ExportVariantType(_)
  | WrapReasonComponent(_)
  | WrapReasonValue(_)
  | WrapVariantLeaf(_) => "1med"
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
  | ExportType({resolvedTypeName, _}) => "ExportType " ++ resolvedTypeName
  | ExportVariantType({name, _}) => "ExportVariantType " ++ name
  | ImportType(importType) =>
    "ImportType " ++ getImportTypeUniqueName(importType)
  | WrapJsComponent({importAnnotation, _}) =>
    "WrapJsComponent " ++ (importAnnotation.importPath |> ImportPath.toString)
  | WrapJsValue({importAnnotation, _}) =>
    "WrapJsValue " ++ (importAnnotation.importPath |> ImportPath.toString)
  | WrapReasonComponent({moduleName, _}) =>
    "WrapReasonComponent " ++ (moduleName |> ModuleName.toString)
  | WrapReasonValue({moduleName, resolvedName, typ, _}) =>
    "WrapReasonValue"
    ++ " resolvedName:"
    ++ resolvedName
    ++ " moduleName:"
    ++ ModuleName.toString(moduleName)
    ++ " typ:"
    ++ EmitTyp.typToString(~language, typ)
  | WrapVariantLeaf({leafName, _}) => "WrapVariantLeaf " ++ leafName
  };