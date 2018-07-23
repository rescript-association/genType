open GenFlowCommon;
let requireModule = moduleName => {
  /* TODO: find the path from the module name */
  let path = Filename.(concat(parent_dir_name, moduleName ++ ".bs"));
  "require(\"" ++ path ++ "\")";
};

module Convert = {
  let apply = (~converter, s) => "/* TODO converter */ " ++ s;
};

type env = {typeMap: StringMap.t(Flow.typ)};

let emitCodeItems = codeItems => {
  let buffer = Buffer.create(100);
  let line = s => Buffer.add_string(buffer, s);
  let codeItem = (env, codeItem) =>
    switch (codeItem) {
    | CodeItem.RawJS(s) =>
      line(s ++ ";\n");
      env;
    | FlowTypeBinding(id, flowType) =>
      line("// type " ++ id ++ " = " ++ Flow.render(flowType) ++ ";\n");
      {typeMap: env.typeMap |> StringMap.add(id, flowType)};
    | FlowAnnotation(annotationBindingName, constructorFlowType) =>
      line("// TODO: FlowAnnotation\n");
      env;
    | ValueBinding(inputModuleName, id, converter) =>
      line(
        "const "
        ++ Ident.name(id)
        ++ " = "
        ++ (inputModuleName |> requireModule |> Convert.apply(~converter))
        ++ "."
        ++ Ident.name(id)
        ++ ";\n",
      );
      env;
    | ConstructorBinding(
        constructorAlias,
        convertableFlowTypes,
        modulePath,
        leafName,
      ) =>
      line("// TODO: ConstructorBinding\n");
      env;
    | ComponentBinding(inputModuleName, flowPropGenerics, id, converter) =>
      line("// TODO: ComponentBinding\n");
      env;
    };
  let _finalEnv =
    List.fold_left(codeItem, {typeMap: StringMap.empty}, codeItems);
  buffer |> Buffer.to_bytes;
};