type exportModuleItems = Hashtbl.t(string, exportModuleItem)
and exportModuleItem = Hashtbl.t(string, exportModuleValue)
and exportModuleValue =
  | S(string)
  | M(exportModuleItems);

let rec emitExportModuleValue = (exportModuleValue, ~buffer) =>
  switch (exportModuleValue) {
  | S(s) => Buffer.add_string(buffer, s)
  | M(exportModuleItems) =>
    exportModuleItems |> emitExportModuleItems(~buffer)
  }
and emitExportModuleItems = (exportModuleItems, ~buffer) => {
  Hashtbl.iter(
    (_moduleName, exportModuleItem) =>
      exportModuleItem |> emitExportModuleItem(~buffer),
    exportModuleItems,
  );
}
and emitExportModuleItem = (exportModuleItem, ~buffer) => {
  Buffer.add_string(buffer, "{ ");

  Hashtbl.fold(
    (fieldName, exportModuleValue, l) => {
      let buffer = Buffer.create(0);
      Buffer.add_string(buffer, fieldName ++ ": ");
      exportModuleValue |> emitExportModuleValue(~buffer);
      [buffer |> Buffer.to_bytes, ...l];
    },
    exportModuleItem,
    [],
  )
  |> List.rev
  |> String.concat(", ")
  |> Buffer.add_string(buffer);

  Buffer.add_string(buffer, " }");
};

let populate = (exportModuleItems: exportModuleItems, ~moduleName) =>
  switch (Hashtbl.find(exportModuleItems, moduleName)) {
  | exportModuleItem => exportModuleItem
  | exception Not_found =>
    let exportModuleItem = Hashtbl.create(1);
    Hashtbl.add(exportModuleItems, moduleName, exportModuleItem);
    exportModuleItem;
  };

let extend =
    (
      exportModuleItems: exportModuleItems,
      ~moduleName,
      ~fieldName,
      ~exportModuleValue: exportModuleValue,
    ) => {
  let exportModuleItem = exportModuleItems |> populate(~moduleName);
  Hashtbl.replace(exportModuleItem, fieldName, exportModuleValue);
};

let rec extendExportModules = (x, ~exportModuleItems, ~valueName) =>
  switch (x) {
  | [] => assert(false)
  | [moduleName, ...rest] =>
    switch (rest) {
    | [] => ()
    | [fieldName] =>
      exportModuleItems
      |> extend(~moduleName, ~fieldName, ~exportModuleValue=S(valueName))
    | [fieldName, _, ..._] =>
      let exportModuleItem = exportModuleItems |> populate(~moduleName);
      let innerExportModuleItems =
        switch (Hashtbl.find(exportModuleItem, fieldName)) {
        | M(innerExportModuleItems) => innerExportModuleItems
        | S(_) => assert(false)
        | exception Not_found =>
          let innerExportModuleItems = Hashtbl.create(1);
          innerExportModuleItems;
        };
      rest
      |> extendExportModules(
           ~exportModuleItems=innerExportModuleItems,
           ~valueName,
         );
      exportModuleItems
      |> extend(
           ~moduleName,
           ~fieldName,
           ~exportModuleValue=M(innerExportModuleItems),
         );
    }
  };

type moduleItemsEmitter = Hashtbl.t(string, exportModuleItem);

let createModuleItemsEmitter = () => Hashtbl.create(1);

let moduleItemToString = ((moduleName, exportModuleItem)) =>
  "export const " ++ moduleName ++ " = " ++ exportModuleItem ++ ";";

let emitAllModuleItems = (~emitters, moduleItemsEmitter) => {
  Hashtbl.fold(
    (moduleName, exportModuleItem, emitters) => {
      let buffer = Buffer.create(0);
      exportModuleItem |> emitExportModuleItem(~buffer);

      let emitters =
        Emitters.export(
          ~emitters,
          (moduleName, buffer |> Buffer.to_bytes) |> moduleItemToString,
        );
      emitters;
    },
    moduleItemsEmitter,
    emitters,
  );
};

let extendExportModules = (~moduleItemsEmitter, resolvedName) =>
  resolvedName
  |> ResolvedName.toList
  |> extendExportModules(
       ~exportModuleItems=moduleItemsEmitter,
       ~valueName=resolvedName |> ResolvedName.toString,
     );