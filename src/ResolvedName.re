type t = list(string);

let dot = (s, x) => x @ [s];

let fromString = x => [x];

let toString = x => x |> String.concat("_");

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
    (fieldNameName, exportModuleValue, l) => {
      let buffer = Buffer.create(0);
      Buffer.add_string(buffer, fieldNameName ++ ": ");
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

let extendExportModules = (~moduleItemsEmitter, x) =>
  x
  |> extendExportModules(
       ~exportModuleItems=moduleItemsEmitter,
       ~valueName=x |> toString,
     );

type eq = (t, t);

module NameSet =
  Set.Make({
    type nonrec t = t;
    let rec compare = (x: t, y: t) =>
      switch (x, y) {
      | ([], []) => 0
      | ([], [_, ..._]) => (-1)
      | ([_, ..._], []) => (-1)
      | ([s1, ...rest1], [s2, ...rest2]) =>
        let n = String.compare(s1, s2);
        n != 0 ? n : compare(rest1, rest2);
      };
  });

let rec applyEquation = (~el: t, eq: eq): list(t) =>
  switch (eq, el) {
  | (([], rhs), _) => [rhs @ el]
  | (([s1, ...rest1], rhs), [s2, ...rest2]) =>
    s1 == s2 ? (rest1, rhs) |> applyEquation(~el=rest2) : []
  | (([_, ..._], _), []) => []
  };

let rec applyEquationsToElements =
        (~eqs: list(eq), ~seen, elements: list(t)): list(eq) => {
  let applyEqs = el => {
    let freshElements =
      eqs
      |> List.map(applyEquation(~el))
      |> List.concat
      |> List.filter(y => !NameSet.mem(y, seen));
    freshElements |> List.map(elFresh => (elFresh, el));
  };

  let newEquations = elements |> List.map(applyEqs) |> List.concat;
  let newElements = newEquations |> List.map(fst);
  let newSeen = NameSet.union(seen, newElements |> NameSet.of_list);

  newEquations == []
    ? newEquations
    : newEquations
      @ (newElements |> applyEquationsToElements(~eqs, ~seen=newSeen));
};

/* Apply equations of the form e.g. X.Y = A from the alias: module A = X.Y.
   Return a list of equations on types.
   E.g. if the element is X.Y.t, return equation A.t = X.Y.t */
let applyEquations = (~eqs: list(eq), el: t): list(eq) =>
  [el] |> applyEquationsToElements(~eqs, ~seen=NameSet.empty);