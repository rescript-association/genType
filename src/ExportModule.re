open GenTypeCommon;

type exportModuleItem = Hashtbl.t(string, exportModuleValue)
and exportModuleValue =
  | S(string)
  | M(exportModuleItem);

type exportModuleItems = Hashtbl.t(string, exportModuleItem);

let rec extendExportModuleItem = (x, ~exportModuleItem, ~valueName) =>
  switch (x) {
  | [] => ()
  | [fieldName] =>
    Hashtbl.replace(exportModuleItem, fieldName, S(valueName))
  | [fieldName, ...rest] =>
    let innerExportModuleItem =
      switch (Hashtbl.find(exportModuleItem, fieldName)) {
      | M(innerExportModuleItem) => innerExportModuleItem
      | S(_) => assert(false)
      | exception Not_found =>
        let innerExportModuleItem = Hashtbl.create(1);
        Hashtbl.replace(
          exportModuleItem,
          fieldName,
          M(innerExportModuleItem),
        );
        innerExportModuleItem;
      };
    rest
    |> extendExportModuleItem(
         ~exportModuleItem=innerExportModuleItem,
         ~valueName,
       );
  };

let extendExportModuleItems =
    (x, ~exportModuleItems: exportModuleItems, ~valueName) =>
  switch (x) {
  | [] => assert(false)
  | [_valueName] => ()
  | [moduleName, ...rest] =>
    let exportModuleItem =
      switch (Hashtbl.find(exportModuleItems, moduleName)) {
      | exportModuleItem => exportModuleItem
      | exception Not_found =>
        let exportModuleItem = Hashtbl.create(1);
        Hashtbl.replace(exportModuleItems, moduleName, exportModuleItem);
        exportModuleItem;
      };
    rest |> extendExportModuleItem(~exportModuleItem, ~valueName);
  };

let createModuleItemsEmitter: unit => exportModuleItems =
  () => Hashtbl.create(1);

let rev_fold = (f, tbl, base) => {
  let list = Hashtbl.fold((k, v, l) => [(k, v), ...l], tbl, []);
  List.fold_left((x, (k, v)) => f(k, v, x), base, list);
};

let rec emitExportModuleValue: exportModuleValue => type_ =
  exportModuleValue =>
    switch (exportModuleValue) {
    | S(s) => ident(s)
    | M(exportModuleItem) =>
      Object(Open, exportModuleItem |> emitExportModuleItem)
    }
and emitExportModuleItem: exportModuleItem => fields =
  exportModuleItem => {
    Hashtbl.fold(
      (fieldName, exportModuleValue, fields) => {
        let field = {
          mutable_: Mutable,
          name: fieldName,
          optional: Mandatory,
          type_: exportModuleValue |> emitExportModuleValue,
        };
        [field, ...fields];
      },
      exportModuleItem,
      [],
    );
  };

let moduleItemToString = ((moduleName, exportModuleItem)) =>
  "export const " ++ moduleName ++ " = " ++ exportModuleItem ++ ";";

let emitAllModuleItems =
    (~config, ~emitters, exportModuleItems: exportModuleItems) => {
  emitters
  |> rev_fold(
       (moduleName, exportModuleItem, emitters) => {
         let fields = exportModuleItem |> emitExportModuleItem;
         let emittedModuleItem =
           Object(Open, fields)
           |> EmitType.typeToString(
                ~config={...config, language: Flow} /* abuse type to print object */,
                ~typeNameIsInterface=_ =>
                false
              );
         "export const "
         ++ moduleName
         ++ " = "
         ++ emittedModuleItem
         ++ ";"
         |> Emitters.export(~emitters);
       },
       exportModuleItems,
     );
};

let extendExportModules =
    (~moduleItemsEmitter: exportModuleItems, resolvedName) =>
  resolvedName
  |> ResolvedName.toList
  |> extendExportModuleItems(
       ~exportModuleItems=moduleItemsEmitter,
       ~valueName=resolvedName |> ResolvedName.toString,
     );