let fileForCmt:
  (~moduleName: string, string, string, string => string) =>
  result(SharedTypes.file, string);
let fullForCmt:
  (
    ~moduleName: string,
    ~allLocations: bool,
    string,
    string,
    string => string
  ) =>
  result(SharedTypes.full, string);
let astForCmt:
  string =>
  result(
    [
      | `Implementation(Parsetree.structure)
      | `Interface(Parsetree.signature)
    ],
    string,
  );
