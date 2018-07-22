open GenFlowCommon;

module StringMap = Map.Make(String);
let requireModule = moduleName => {
  /* TODO: find the path from the module name */
  let path = Filename.(concat(parent_dir_name, moduleName ++ ".bs"));
  "require(\"" ++ path ++ "\")";
};

let applyConverter = (~converter, s) => "/* TODO converter */ " ++ s;

let codeItems = codeItems => {
  let buffer = Buffer.create(100);
  let line = s => Buffer.add_string(buffer, s);
  let codeItem = (typeMap, codeItem) =>
    switch (codeItem) {
    | CodeItem.RawJS(s) =>
      line(s ++ ";\n");
      typeMap;
    | FlowTypeBinding(id, flowType) =>
      line("// type " ++ id ++ " = " ++ Flow.render(flowType) ++ ";\n");
      typeMap |> StringMap.add(id, flowType);
    | FlowAnnotation(annotationBindingName, constructorFlowType) =>
      line("// TODO: FlowAnnotation\n");
      typeMap;
    | ValueBinding(inputModuleName, id, converter) =>
      line(
        "const "
        ++ Ident.name(id)
        ++ " = "
        ++ (inputModuleName |> requireModule |> applyConverter(~converter))
        ++ "."
        ++ Ident.name(id)
        ++ ";\n",
      );
      typeMap;
    | ConstructorBinding(
        constructorAlias,
        convertableFlowTypes,
        modulePath,
        leafName,
      ) =>
      line("// TODO: ConstructorBinding\n");
      typeMap;
    | ComponentBinding(inputModuleName, flowPropGenerics, id, converter) =>
      line("// TODO: ComponentBinding\n");
      typeMap;
    };
  let _typeMap = List.fold_left(codeItem, StringMap.empty, codeItems);
  buffer |> Buffer.to_bytes;
};