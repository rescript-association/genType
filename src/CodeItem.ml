open GenTypeCommon

type exportType = {
  loc : Location.t;
  nameAs : string option;
  opaque : bool option;
  type_ : type_;
  typeVars : string list;
  resolvedTypeName : ResolvedName.t;
}

type importComponent = {
  asPath : string;
  childrenTyp : type_;
  exportType : exportType;
  importAnnotation : Annotation.import;
  propsFields : fields;
  propsTypeName : string;
}

type importValue = {
  asPath : string;
  importAnnotation : Annotation.import;
  type_ : type_;
  valueName : string;
}

type exportComponent = {
  componentAccessPath : Runtime.moduleAccessPath;
  exportType : exportType;
  moduleAccessPath : Runtime.moduleAccessPath;
  nestedModuleName : ModuleName.t option;
  type_ : type_;
}

type exportValue = {
  docString : string;
  moduleAccessPath : Runtime.moduleAccessPath;
  originalName : string;
  resolvedName : ResolvedName.t;
  type_ : type_;
}

type exportFromTypeDeclaration = {
  exportType : exportType;
  annotation : Annotation.t;
}

type importType = {
  typeName : string;
  asTypeName : string option;
  importPath : ImportPath.t;
}

type exportTypeItem = {
  typeVars : string list;
  type_ : type_;
  annotation : Annotation.t;
}

type exportTypeMap = exportTypeItem StringMap.t

type typeDeclaration = {
  exportFromTypeDeclaration : exportFromTypeDeclaration;
  importTypes : importType list;
}

type t =
  | ExportComponent of exportComponent
  | ExportValue of exportValue
  | ImportComponent of importComponent
  | ImportValue of importValue

type translation = {
  importTypes : importType list;
  codeItems : t list;
  typeDeclarations : typeDeclaration list;
}
