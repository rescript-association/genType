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

let emitExportType =
    (~language, {CodeItem.opaque, typeParams, typeName, typ}) =>
  typ
  |> EmitTyp.toString(~language)
  |> EmitTyp.emitExportType(
       ~language,
       ~opaque,
       ~typeName,
       ~typeParams=
         EmitTyp.genericsString(
           List.map(EmitTyp.toString(~language), typeParams),
         ),
     );

let emitExportUnionType = (~language, {CodeItem.typeParams, leafTypes, name}) =>
  "export type "
  ++ name
  ++ EmitTyp.genericsString(
       List.map(EmitTyp.toString(~language), typeParams),
     )
  ++ " =\n  | "
  ++ String.concat(
       "\n  | ",
       List.map(EmitTyp.toString(~language), leafTypes),
     );

let emitImportType = (~language, importType) =>
  switch (importType) {
  | CodeItem.ImportComment(s) => s
  | ImportAsFrom(typeName, asTypeName, importPath) =>
    EmitTyp.importTypeAs(~language, ~typeName, ~asTypeName, ~importPath)
  };

let emitCodeItems = (~language, ~outputFileRelative, ~resolver, codeItems) => {
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
      EmitTyp.commentBeforeRequire(~language)
      ++ "const "
      ++ ModuleName.toString(moduleName)
      ++ " = require(\""
      ++ (importPath |> ImportPath.toString)
      ++ "\");",
    );
  let emitCheckJsWrapperType = (~env, ~propsTypeName) =>
    switch (env.externalReactClass) {
    | [] => ()

    | [{componentName}] =>
      let s =
        "export function checkJsWrapperType(props: "
        ++ propsTypeName
        ++ ") {\n      return <"
        ++ componentName
        ++ " {...props}/>;\n    }";
      line(s);

    | [_, ..._] =>
      line(
        "// genFlow warning: found more than one external component annotated with @genFlow",
      )
    };
  let emitCodeItem = (env, codeItem) => {
    if (Debug.codeItems) {
      logItem("Code Item: %s\n", codeItem |> CodeItem.toString(~language));
    };
    switch (codeItem) {
    | CodeItem.ImportType(importType) =>
      line(emitImportType(~language, importType) ++ ";");
      env;

    | ExportType(exportType) =>
      line(emitExportType(~language, exportType) ++ ";");
      env;

    | ExportUnionType(exportUnionType) =>
      line(emitExportUnionType(~language, exportUnionType) ++ ";");
      env;

    | ValueBinding(moduleName, id, typ, converter) =>
      let importPath =
        ModuleResolver.resolveModule(
          ~outputFileRelative,
          ~resolver,
          ~importExtension=".bs",
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
        ++ EmitTyp.toString(~language, typ)
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
      line(emitExportType(~language, exportType) ++ ";");
      let recordAsInt = recordValue |> Runtime.emitRecordAsInt(~language);
      if (convertableTypes == []) {
        line(
          "export const "
          ++ variantName
          ++ ": "
          ++ EmitTyp.toString(~language, constructorType)
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
        let mkReturn = s => "return " ++ s;
        let mkBody = args =>
          recordValue
          |> Runtime.emitRecordAsBlock(~language, ~args)
          |> mkReturn;
        line(
          "export const "
          ++ variantName
          ++ ": "
          ++ EmitTyp.toString(~language, constructorType)
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

    | ComponentBinding({exportType, moduleName, propsTypeName, converter}) =>
      let importPath =
        ModuleResolver.resolveModule(
          ~outputFileRelative,
          ~resolver,
          ~importExtension=".bs",
          moduleName,
        );
      let moduleNameBs = moduleName |> ModuleName.forBsFile;

      let name = EmitTyp.componentExportName(~language, ~moduleName);
      let jsProps = "jsProps";
      let jsPropsDot = s => jsProps ++ "." ++ s;
      let componentType =
        Ident(
          EmitTyp.reactComponentType(~language),
          [Ident(propsTypeName, [])],
        );
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

      line(emitExportType(~language, exportType) ++ ";");
      line(
        "export const "
        ++ name
        ++ ": "
        ++ EmitTyp.toString(~language, componentType)
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

      emitCheckJsWrapperType(~env, ~propsTypeName);

      let requiresWithModule =
        moduleNameBs
        |> requireModule(
             ~requires=env.requires,
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
      let requires =
        env.requires
        |> ModuleNameMap.add(
             ModuleName.fromStringUnsafe(componentName),
             importPath,
           );
      {
        ...env,
        requires,
        externalReactClass: [externalReactClass, ...env.externalReactClass],
      };
    };
  };
  let {requires, externalReactClass} =
    List.fold_left(
      emitCodeItem,
      {
        requires: ModuleNameMap.empty,
        typeMap: StringMap.empty,
        externalReactClass: [],
      },
      codeItems,
    );

  if (externalReactClass != []) {
    if (EmitTyp.requireReact(~language)) {
      emitRequire(ModuleName.react, ImportPath.react);
    } else {
      line_(requireBuffer, EmitTyp.importReact(~language));
    };
  };
  requires |> ModuleNameMap.iter(emitRequire);

  [
    requireBuffer |> Buffer.to_bytes,
    mainBuffer |> Buffer.to_bytes,
    exportBuffer |> Buffer.to_bytes,
  ]
  |> String.concat("\n\n");
};