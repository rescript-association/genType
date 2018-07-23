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
  requires: StringMap.t(string),
  typeMap: StringMap.t(Flow.typ),
  exports: list((string, option(Flow.typ))),
};

let emitCodeItems = codeItems => {
  let requireBuffer = Buffer.create(100);
  let mainBuffer = Buffer.create(100);
  let exportBuffer = Buffer.create(100);
  let line_ = (buffer, s) => {
    Buffer.add_string(buffer, s);
    Buffer.add_string(buffer, "\n");
  };
  let line = line_(mainBuffer);
  let emitExport = ((id, flowTypeOpt)) => {
    let addType = s =>
      switch (flowTypeOpt) {
      | None => s
      | Some(flowType) => "(" ++ s ++ ": " ++ Flow.render(flowType) ++ ")"
      };
    line_(exportBuffer, "exports." ++ id ++ " = " ++ addType(id) ++ ";");
  };
  let emitRequire = (id, v) =>
    line_(requireBuffer, "var " ++ id ++ " = require(\"" ++ v ++ "\");");
  let codeItem = (env, codeItem) =>
    switch (codeItem) {
    | CodeItem.RawJS(s) =>
      line(s ++ ";");
      env;
    | FlowTypeBinding(id, flowType) => {
        ...env,
        typeMap: env.typeMap |> StringMap.add(id, flowType),
      }
    | FlowAnnotation(annotationBindingName, constructorFlowType) =>
      line("// TODO: FlowAnnotation");
      env;
    | ValueBinding(inputModuleName, id, converter) =>
      let name = Ident.name(id);
      line(
        "const "
        ++ name
        ++ " = "
        ++ (inputModuleName |> requireModule |> Convert.apply(~converter))
        ++ "."
        ++ name
        ++ ";",
      );
      let flowType = env.typeMap |> StringMap.find(name);
      {
        ...env,
        exports: [(Ident.name(id), Some(flowType)), ...env.exports],
      };
    | ConstructorBinding(
        constructorAlias,
        convertableFlowTypes,
        modulePath,
        leafName,
      ) =>
      line("// TODO: ConstructorBinding");
      env;
    | ComponentBinding(inputModuleName, flowPropGenerics, id, converter) =>
      line("const make = ReasonReact.wrapReasonForJs(");
      line("// TODO: ComponentBinding");
      line("    );");
      {
        ...env,
        exports: [(Ident.name(id), flowPropGenerics), ...env.exports],
        requires:
          env.requires
          |> StringMap.add("ReasonReact", "reason-react/src/ReasonReact.js"),
      };
    };
  let {requires, exports} =
    List.fold_left(
      codeItem,
      {typeMap: StringMap.empty, exports: [], requires: StringMap.empty},
      codeItems,
    );
  requires |> StringMap.iter(emitRequire);
  exports |> List.rev |> List.iter(emitExport);
  "\n"
  ++ (requireBuffer |> Buffer.to_bytes)
  ++ "\n"
  ++ (mainBuffer |> Buffer.to_bytes)
  ++ "\n"
  ++ (exportBuffer |> Buffer.to_bytes);
};