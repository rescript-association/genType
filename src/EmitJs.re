open GenFlowCommon;
let requireModule = moduleName => {
  /* TODO: find the path from the module name */
  let path = Filename.(concat(parent_dir_name, moduleName ++ ".bs"));
  "require(\"" ++ path ++ "\")";
};

module Convert = {
  let apply = (~converter, s) => "/* TODO converter */ " ++ s;
};

type env = {
  typeMap: StringMap.t(Flow.typ),
  exports: list((string, Flow.typ)),
};

let emitCodeItems = codeItems => {
  let buffer = Buffer.create(100);
  let line = s => Buffer.add_string(buffer, s);
  let emitExport = ((id, flowType)) =>
    line(
      "exports."
      ++ id
      ++ " = ("
      ++ id
      ++ ": "
      ++ Flow.render(flowType)
      ++ ");\n",
    );
  let codeItem = (env, codeItem) =>
    switch (codeItem) {
    | CodeItem.RawJS(s) =>
      line(s ++ ";\n");
      env;
    | FlowTypeBinding(id, flowType) =>
      {...env, typeMap: env.typeMap |> StringMap.add(id, flowType)};
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
      let flowType = env.typeMap |> StringMap.find(Ident.name(id));
      {...env, exports: [(Ident.name(id), flowType), ...env.exports]};
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
  let {exports} =
    List.fold_left(
      codeItem,
      {typeMap: StringMap.empty, exports: []},
      codeItems,
    );
  exports |> List.iter(emitExport);
  buffer |> Buffer.to_bytes;
};