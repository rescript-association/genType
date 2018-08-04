open GenFlowCommon;

type env = {
  requires: ModuleNameMap.t(ImportPath.t),
  typeMap: StringMap.t(typ),
  externalReactClass: list(CodeItem.externalReactClass),
};

let requireModule =
    (~requires, ~outputFileRelative, ~resolver, ~importPath, moduleName) =>
  requires
  |> ModuleNameMap.add(
       moduleName,
       moduleName
       |> ModuleResolver.resolveSourceModule(
            ~outputFileRelative,
            ~resolver,
            ~importPath,
          ),
     );

let emitExportType = ({CodeItem.opaque, typeParams, typeName, typ}) =>
  typ
  |> EmitTyp.toString
  |> EmitTyp.emitExportType(
       ~opaque,
       ~typeName,
       ~typeParams=
         EmitTyp.genericsString(List.map(EmitTyp.toString, typeParams)),
     );

let emitExportUnionType = ({CodeItem.typeParams, leafTypes, name}) =>
  "export type "
  ++ name
  ++ EmitTyp.genericsString(List.map(EmitTyp.toString, typeParams))
  ++ " =\n  | "
  ++ String.concat("\n  | ", List.map(EmitTyp.toString, leafTypes));

let emitImport = import =>
  switch (import) {
  | CodeItem.ImportComment(s) => s
  | ImportAsFrom(typeName, asTypeName, importPath) =>
    "import type {"
    ++ typeName
    ++ (
      switch (asTypeName) {
      | Some(asT) => " as " ++ asT
      | None => ""
      }
    )
    ++ "} from '"
    ++ (importPath |> ImportPath.toString)
    ++ "'"
  };

let emitCodeItems = (~outputFileRelative, ~resolver, codeItems) => {
  let requireBuffer = Buffer.create(100);
  let mainBuffer = Buffer.create(100);
  let exportBuffer = Buffer.create(100);
  let line_ = (buffer, s) => {
    if (Buffer.length(buffer) > 0) {
      Buffer.add_string(buffer, "\n");
    };
    Buffer.add_string(buffer, s);
  };
  let line = line_(mainBuffer);
  let emitRequire = (moduleName, importPath) =>
    line_(
      requireBuffer,
      EmitTyp.commentBeforeRequire
      ++ "const "
      ++ ModuleName.toString(moduleName)
      ++ " = require(\""
      ++ (importPath |> ImportPath.toString)
      ++ "\");",
    );
  let emitCheckJsWrapperType = (~env, ~propsTypeName) =>
    switch (env.externalReactClass) {
    | [] => env.requires

    | [{componentName}] =>
      let s =
        "function checkJsWrapperType(props: "
        ++ propsTypeName
        ++ ") {\n      return <"
        ++ componentName
        ++ " {...props}> </"
        ++ componentName
        ++ ">;\n    }";
      line(s);
      EmitTyp.requireReact ?
        env.requires
        |> ModuleNameMap.add(ModuleName.react, ImportPath.reactjs) :
        env.requires;

    | [_, ..._] =>
      line(
        "// genFlow warning: found more than one external component annotated with @genFlow",
      );
      env.requires;
    };
  let emitCodeItem = (env, codeItem) => {
    if (Debug.codeItems) {
      logItem("Code Item: %s\n", codeItem |> CodeItem.toString);
    };
    switch (codeItem) {
    | CodeItem.ImportType(import) =>
      line(emitImport(import) ++ ";");
      env;

    | ExportType(exportType) =>
      line(emitExportType(exportType) ++ ";");
      env;

    | ExportUnionType(exportUnionType) =>
      line(emitExportUnionType(exportUnionType) ++ ";");
      env;

    | ValueBinding(moduleName, id, typ, converter) =>
      let importPath =
        ModuleResolver.resolveModule(
          ~outputFileRelative,
          ~resolver,
          ~ext=".bs",
          moduleName,
        );
      let moduleNameBs = moduleName |> ModuleName.forBsFile;
      let requires =
        moduleNameBs
        |> requireModule(
             ~requires=env.requires,
             ~outputFileRelative,
             ~resolver,
             ~importPath,
           );
      line(
        "export const "
        ++ id
        ++ ": "
        ++ EmitTyp.toString(typ)
        ++ " = "
        ++ (
          ModuleName.toString(moduleNameBs)
          ++ "."
          ++ id
          |> Convert.toJS(~converter)
        )
        ++ ";",
      );
      {...env, typeMap: env.typeMap |> StringMap.add(id, typ), requires};

    | ConstructorBinding(
        exportType,
        constructorType,
        convertableTypes,
        variantName,
        recordValue,
      ) =>
      line(emitExportType(exportType) ++ ";");
      let recordAsInt = recordValue |> Runtime.emitRecordAsInt;
      if (convertableTypes == []) {
        line(
          "export const "
          ++ variantName
          ++ ": "
          ++ EmitTyp.toString(constructorType)
          ++ " = "
          ++ recordAsInt
          ++ ";",
        );
      } else {
        let args =
          convertableTypes
          |> List.mapi((i, (converter, _flowTyp)) => {
               let arg = Emit.argi(i + 1);
               let v = arg |> Convert.toReason(~converter);
               (arg, v);
             });
        let mkBody = args => recordValue |> Runtime.emitRecordAsBlock(~args);
        line(
          "export const "
          ++ variantName
          ++ ": "
          ++ EmitTyp.toString(constructorType)
          ++ " = "
          ++ Emit.funDef(~args, ~mkBody, ""),
        );
      };
      {
        ...env,
        requires:
          env.requires
          |> ModuleNameMap.add(
               ModuleName.createBucklescriptBlock,
               ImportPath.bsPlatformBlock,
             ),
      };

    | ComponentBinding({
        exportType,
        moduleName,
        componentType,
        propsTypeName,
        converter,
      }) =>
      let importPath =
        ModuleResolver.resolveModule(
          ~outputFileRelative,
          ~resolver,
          ~ext=".bs",
          moduleName,
        );
      let moduleNameBs = moduleName |> ModuleName.forBsFile;

      let name = "component";
      let jsProps = "jsProps";
      let jsPropsDot = s => jsProps ++ "." ++ s;
      let componentType =
        Ident(EmitTyp.reactComponentType, [Ident(propsTypeName, [])]);
      let args =
        switch (converter) {
        | Fn((groupedArgConverters, _retConverter)) =>
          switch (groupedArgConverters) {
          | [
              GroupConverter(propConverters),
              ArgConverter(_, childrenConverter),
              ..._,
            ] =>
            (
              propConverters
              |> List.map(((s, _optionalness, argConverter)) =>
                   jsPropsDot(s)
                   |> Convert.apply(~converter=argConverter, ~toJS=true)
                 )
            )
            @ [
              jsPropsDot("children")
              |> Convert.apply(~converter=childrenConverter, ~toJS=true),
            ]

          | [ArgConverter(_, childrenConverter), ..._] => [
              jsPropsDot("children")
              |> Convert.apply(~converter=childrenConverter, ~toJS=true),
            ]

          | _ => [jsPropsDot("children")]
          }

        | _ => [jsPropsDot("children")]
        };

      line(emitExportType(exportType) ++ ";");
      line(
        "export const "
        ++ name
        ++ ": "
        ++ EmitTyp.toString(componentType)
        ++ " = ReasonReact.wrapReasonForJs(",
      );
      line("  " ++ ModuleName.toString(moduleNameBs) ++ ".component" ++ ",");
      line("  (function _(" ++ jsProps ++ ": " ++ propsTypeName ++ ") {");
      line(
        "     return "
        ++ ModuleName.toString(moduleNameBs)
        ++ "."
        ++ "make"
        ++ Emit.parens(args)
        ++ ";",
      );
      line("  }));");

      let requiresWithCheckJsWrapper =
        emitCheckJsWrapperType(~env, ~propsTypeName);

      let requiresWithModule =
        moduleNameBs
        |> requireModule(
             ~requires=requiresWithCheckJsWrapper,
             ~outputFileRelative,
             ~resolver,
             ~importPath,
           );
      let requiresWithReasonReact =
        requiresWithModule
        |> ModuleNameMap.add(ModuleName.reasonReact, ImportPath.reasonReact);
      {
        ...env,
        typeMap: env.typeMap |> StringMap.add("component", componentType),
        requires: requiresWithReasonReact,
      };

    | ExternalReactClass({componentName, importPath} as externalReactClass) =>
      line(
        "const "
        ++ componentName
        ++ " = require(\""
        ++ (importPath |> ImportPath.toString)
        ++ "\");"
        ++ " // external "
        ++ componentName
        ++ " : ReasonReact.reactClass = \""
        ++ (importPath |> ImportPath.toString)
        ++ "\"",
      );
      {
        ...env,
        externalReactClass: [externalReactClass, ...env.externalReactClass],
      };
    };
  };
  let {requires, _} =
    List.fold_left(
      emitCodeItem,
      {
        requires: ModuleNameMap.empty,
        typeMap: StringMap.empty,
        externalReactClass: [],
      },
      codeItems,
    );

  requires |> ModuleNameMap.iter(emitRequire);

  [
    requireBuffer |> Buffer.to_bytes,
    mainBuffer |> Buffer.to_bytes,
    exportBuffer |> Buffer.to_bytes,
  ]
  |> String.concat("\n\n");
};