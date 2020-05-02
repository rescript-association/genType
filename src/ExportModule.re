open GenTypeCommon;

type exportModuleItem = Hashtbl.t(string, exportModuleValue)
and exportModuleValue =
  | S(string, type_, Converter.t)
  | M(exportModuleItem);

type exportModuleItems = Hashtbl.t(string, exportModuleItem);

type types = {
  typeForValue: type_,
  typeForType: type_,
  needsConversion: bool,
};

type fieldInfo = {
  fieldForValue: field,
  fieldForType: field,
  needsConversion: bool,
};

let rec exportModuleValueToType = exportModuleValue =>
  switch (exportModuleValue) {
  | S(s, type_, converter) => {
      typeForValue: ident(s),
      typeForType: type_,
      needsConversion: converter != IdentC,
    }
  | M(exportModuleItem) =>
    let fieldsInfo = exportModuleItem |> exportModuleItemToFields;
    let fieldsForValue =
      fieldsInfo |> List.map(({fieldForValue}) => fieldForValue);
    let fieldsForType =
      fieldsInfo |> List.map(({fieldForType}) => fieldForType);
    let needsConversion =
      fieldsInfo
      |> List.fold_left(
           (acc, {needsConversion}) => acc || needsConversion,
           false,
         );
    {
      typeForValue: Object(Open, fieldsForValue),
      typeForType: Object(Open, fieldsForType),
      needsConversion,
    };
  }
and exportModuleItemToFields: exportModuleItem => list(fieldInfo) =
  exportModuleItem => {
    Hashtbl.fold(
      (fieldName, exportModuleValue, fields) => {
        let {typeForValue, typeForType, needsConversion} =
          exportModuleValue |> exportModuleValueToType;
        let fieldForType = {
          mutable_: Mutable,
          nameJS: fieldName,
          nameRE: fieldName,
          optional: Mandatory,
          type_: typeForType,
        };
        let fieldForValue = {...fieldForType, type_: typeForValue};
        [{fieldForValue, fieldForType, needsConversion}, ...fields];
      },
      exportModuleItem,
      [],
    );
  };

let rec extendExportModuleItem =
        (
          x,
          ~converter,
          ~exportModuleItem: exportModuleItem,
          ~type_,
          ~valueName,
        ) =>
  switch (x) {
  | [] => ()
  | [fieldName] =>
    Hashtbl.replace(
      exportModuleItem,
      fieldName,
      S(valueName, type_, converter),
    )
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
         ~converter,
         ~exportModuleItem=innerExportModuleItem,
         ~valueName,
         ~type_,
       );
  };

let extendExportModuleItems =
    (x, ~converter, ~exportModuleItems: exportModuleItems, ~type_, ~valueName) =>
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
    rest
    |> extendExportModuleItem(
         ~converter,
         ~exportModuleItem,
         ~type_,
         ~valueName,
       );
  };

let createModuleItemsEmitter: unit => exportModuleItems =
  () => Hashtbl.create(1);

let rev_fold = (f, tbl, base) => {
  let list = Hashtbl.fold((k, v, l) => [(k, v), ...l], tbl, []);
  List.fold_left((x, (k, v)) => f(k, v, x), base, list);
};

let emitAllModuleItems =
    (~config, ~emitters, ~fileName, exportModuleItems: exportModuleItems) => {
  emitters
  |> rev_fold(
       (moduleName, exportModuleItem, emitters) => {
         let {typeForValue, typeForType, needsConversion} =
           M(exportModuleItem) |> exportModuleValueToType;
         if (Debug.codeItems^) {
           Log_.item(
             "EmitModule %s needsConversion:%b@.",
             moduleName,
             needsConversion,
           );
         };
         if (needsConversion) {
           emitters;
         } else {
           let emittedModuleItem =
             config.modulesAsObjects
               ? ModuleName.forInnerModule(
                   ~fileName,
                   ~innerModuleName=moduleName,
                 )
                 |> ModuleName.toString
               : typeForValue
                 |> EmitType.typeToString(
                      ~config={...config, language: Flow} /* abuse type to print object */,
                      ~typeNameIsInterface=_ =>
                      false
                    );
           emittedModuleItem
           |> EmitType.emitExportConst(
                ~config,
                ~emitters,
                ~name=moduleName,
                ~type_=typeForType,
                ~typeNameIsInterface=_ =>
                false
              );
         };
       },
       exportModuleItems,
     );
};

let extendExportModules =
    (~converter, ~moduleItemsEmitter: exportModuleItems, ~type_, resolvedName) =>
  resolvedName
  |> ResolvedName.toList
  |> extendExportModuleItems(
       ~converter,
       ~exportModuleItems=moduleItemsEmitter,
       ~type_,
       ~valueName=resolvedName |> ResolvedName.toString,
     );