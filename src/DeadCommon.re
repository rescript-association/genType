/* Adapted from https://github.com/LexiFi/dead_code_analyzer */

let reportUnderscore = ref(false);

let active = Sys.getenv_opt("DCE") != None;

let analyzeTypes = true;

let analyzeExternals = false;

let transitive = true;

let write = Sys.getenv_opt("Write") != None;

let verbose = Sys.getenv_opt("Debug") != None;

let deadAnnotation = "dead";

/* Location printer: `filename:line: ' */
let posToString = (~printCol=false, ~shortFile=false, pos: Lexing.position) => {
  let file = pos.Lexing.pos_fname;
  let line = pos.Lexing.pos_lnum;
  let col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol;
  (shortFile ? file |> Filename.basename : file)
  ++ ":"
  ++ string_of_int(line)
  ++ (printCol ? ":" ++ string_of_int(col) : ": ");
};

/********   ATTRIBUTES   ********/
module PosSet =
  Set.Make({
    type t = Lexing.position;
    let compare = compare;
  });

module PosHash = {
  include Hashtbl.Make({
    type t = Lexing.position;

    let hash = x => {
      let s = Filename.basename(x.Lexing.pos_fname);
      Hashtbl.hash((x.Lexing.pos_cnum, s));
    };

    let equal = (x: t, y) => x == y;
  });

  let findSet = (h, k) =>
    try(find(h, k)) {
    | Not_found => PosSet.empty
    };

  let addSet = (h, k, v) => {
    let set = findSet(h, k);
    replace(h, k, PosSet.add(v, set));
  };

  let mergeSet = (table, pos1, pos2) => {
    let set1 = findSet(table, pos1);
    let set2 = findSet(table, pos2);
    let setUnion = PosSet.union(set1, set2);
    replace(table, pos1, setUnion);
    if (verbose) {
      GenTypeCommon.logItem(
        "mergeSet %s --> %s\n",
        pos2 |> posToString(~printCol=true, ~shortFile=true),
        pos1 |> posToString(~printCol=true, ~shortFile=true),
      );
    };
  };
};

module FileSet = Set.Make(String);

module FileHash = {
  include Hashtbl.Make({
    type t = string;

    let hash = (x: t) => Hashtbl.hash(x);

    let equal = (x: t, y) => x == y;
  });

  let findSet = (table, key) =>
    try(find(table, key)) {
    | Not_found => FileSet.empty
    };

  let addFile = (table, key) => {
    let set = findSet(table, key);
    replace(table, key, set);
  };

  let addSet = (table, key, value) => {
    let set = findSet(table, key);
    replace(table, key, FileSet.add(value, set));
  };
};

type decs = Hashtbl.t(Lexing.position, string);
let valueDecs: decs = Hashtbl.create(256); /* all exported value declarations */
let typeDecs: decs = Hashtbl.create(256);

let valueReferences: PosHash.t(PosSet.t) = PosHash.create(256); /* all value references */

let fileReferences: FileHash.t(FileSet.t) = FileHash.create(256);

let fields: Hashtbl.t(string, Lexing.position) = (
  Hashtbl.create(256): Hashtbl.t(string, Lexing.position)
); /* link from fields (record/variant) paths and locations */

let lastPos = ref(Lexing.dummy_pos); /* helper to diagnose occurrences of Location.none in the typedtree */
let currentSrc = ref("");
let currentBindingPos = ref(Lexing.dummy_pos);

let none_ = "_none_";
let include_ = "*include*";

/********   HELPERS   ********/

let addValueReference = (~addFileReference, posDeclaration, posUsage) => {
  let posUsage =
    !transitive || currentBindingPos^ == Lexing.dummy_pos
      ? posUsage : currentBindingPos^;
  if (verbose) {
    GenTypeCommon.logItem(
      "addValueReference %s --> %s\n",
      posUsage |> posToString(~printCol=true, ~shortFile=true),
      posDeclaration |> posToString(~printCol=true, ~shortFile=true),
    );
  };
  PosHash.addSet(valueReferences, posDeclaration, posUsage);
  if (addFileReference
      && posDeclaration.pos_fname != none_
      && posUsage.pos_fname != none_
      && posUsage.pos_fname != posDeclaration.pos_fname) {
    FileHash.addSet(
      fileReferences,
      posUsage.pos_fname,
      posDeclaration.pos_fname,
    );
  };
};

let iterFilesFromRootsToLeaves = iterFun => {
  /* For each file, the number of incoming references */
  let inverseReferences: Hashtbl.t(string, int) = Hashtbl.create(1);
  /* For each number of incoming references, the files */
  let referencesByNumber: Hashtbl.t(int, FileSet.t) = Hashtbl.create(1);

  let getNum = fileName =>
    try(Hashtbl.find(inverseReferences, fileName)) {
    | Not_found => 0
    };

  let getSet = num =>
    try(Hashtbl.find(referencesByNumber, num)) {
    | Not_found => FileSet.empty
    };

  let addIncomingEdge = fileName => {
    let oldNum = getNum(fileName);
    let newNum = oldNum + 1;
    let oldSetAtNum = getSet(oldNum);
    let newSetAtNum = FileSet.remove(fileName, oldSetAtNum);
    let oldSetAtNewNum = getSet(newNum);
    let newSetAtNewNum = FileSet.add(fileName, oldSetAtNewNum);
    Hashtbl.replace(inverseReferences, fileName, newNum);
    Hashtbl.replace(referencesByNumber, oldNum, newSetAtNum);
    Hashtbl.replace(referencesByNumber, newNum, newSetAtNewNum);
  };

  let removeIncomingEdge = fileName => {
    let oldNum = getNum(fileName);
    let newNum = oldNum - 1;
    let oldSetAtNum = getSet(oldNum);
    let newSetAtNum = FileSet.remove(fileName, oldSetAtNum);
    let oldSetAtNewNum = getSet(newNum);
    let newSetAtNewNum = FileSet.add(fileName, oldSetAtNewNum);
    Hashtbl.replace(inverseReferences, fileName, newNum);
    Hashtbl.replace(referencesByNumber, oldNum, newSetAtNum);
    Hashtbl.replace(referencesByNumber, newNum, newSetAtNewNum);
  };

  let isSourceFile = fileName => FileHash.mem(fileReferences, fileName);

  let addEdge = (fromFile, toFile) =>
    if (isSourceFile(fromFile)) {
      addIncomingEdge(toFile);
    };

  let removeEdge = (fromFile, toFile) =>
    if (isSourceFile(fromFile)) {
      removeIncomingEdge(toFile);
    };

  fileReferences
  |> FileHash.iter((fromFile, set) => {
       if (getNum(fromFile) == 0) {
         Hashtbl.replace(
           referencesByNumber,
           0,
           FileSet.add(fromFile, getSet(0)),
         );
       };
       set |> FileSet.iter(toFile => {addEdge(fromFile, toFile)});
     });

  while (getSet(0) != FileSet.empty) {
    let filesWithNoIncomingReferences = getSet(0);
    Hashtbl.remove(referencesByNumber, 0);
    filesWithNoIncomingReferences
    |> FileSet.iter(fileName => {
         iterFun(fileName);
         let references =
           try(FileHash.find(fileReferences, fileName)) {
           | Not_found => FileSet.empty
           };
         references |> FileSet.iter(toFile => removeEdge(fileName, toFile));
       });
  };
};

let getModuleName = fn => fn |> Paths.getModuleName |> ModuleName.toString;

let checkUnderscore = name => reportUnderscore^ || name.[0] != '_';

let hashtblAddToList = (hashtbl, key, elt) => Hashtbl.add(hashtbl, key, elt);

/********   PROCESSING  ********/

let export = (~path, ~moduleName, ~decs: decs, ~id, ~loc) => {
  let value =
    String.concat(".", List.rev_map(Ident.name, path))
    ++ "."
    ++ id.Ident.name;
  let pos = loc.Location.loc_start;

  /* a .cmi file can contain locations from other files.
       For instance:
           module M : Set.S with type elt = int
       will create value definitions whose location is in set.mli
     */
  if (!loc.loc_ghost
      && (
        moduleName == getModuleName(pos.pos_fname) || moduleName === include_
      )
      && checkUnderscore(id.name)) {
    if (verbose) {
      GenTypeCommon.logItem(
        "export %s %s\n",
        id.name,
        pos |> posToString(~printCol=true, ~shortFile=true),
      );
    };

    hashtblAddToList(decs, pos, value);
  };
};

/**** REPORTING ****/

let pathWithoutHead = path => {
  let rec cutFromNextDot = (s, pos) =>
    if (pos == String.length(s)) {
      s;
    } else if (s.[pos] == '.') {
      String.sub(s, pos + 1, String.length(s) - pos - 1);
    } else {
      cutFromNextDot(s, pos + 1);
    };
  cutFromNextDot(path, 0);
};

/* Keep track of the location of values exported via genType */
module ProcessDeadAnnotations = {
  /* Positions exported to JS */
  let positionsAnnotatedWithGenType = PosHash.create(1);
  let positionsAnnotatedDead = PosHash.create(1);

  let isAnnotatedDead = pos => PosHash.mem(positionsAnnotatedDead, pos);

  let isAnnotatedGentypeOrDead = pos =>
    PosHash.mem(positionsAnnotatedWithGenType, pos) || isAnnotatedDead(pos);

  let posAnnotatedWithGenType = (pos: Lexing.position) => {
    PosHash.replace(positionsAnnotatedWithGenType, pos, ());
  };

  let posAnnotatedDead = (pos: Lexing.position) => {
    PosHash.replace(positionsAnnotatedDead, pos, ());
  };

  let processAttributes = (~ignoreInterface, ~pos, attributes) => {
    if (attributes |> Annotation.hasGenTypeAnnotation(~ignoreInterface)) {
      pos |> posAnnotatedWithGenType;
    };
    if (attributes
        |> Annotation.getAttributePayload((==)(deadAnnotation)) != None) {
      pos |> posAnnotatedDead;
    };
  };

  let collectExportLocations = (~ignoreInterface) => {
    let super = Tast_mapper.default;
    let value_binding =
        (
          self,
          {vb_attributes, vb_pat} as value_binding: Typedtree.value_binding,
        ) => {
      switch (vb_pat.pat_desc) {
      | Tpat_var(id, pLoc) =>
        vb_attributes
        |> processAttributes(~ignoreInterface, ~pos=pLoc.loc.loc_start)

      | _ => ()
      };
      super.value_binding(self, value_binding);
    };
    let type_kind = (self, typeKind: Typedtree.type_kind) => {
      switch (typeKind) {
      | Ttype_record(labelDeclarations) =>
        labelDeclarations
        |> List.iter(({ld_attributes, ld_loc}: Typedtree.label_declaration) =>
             ld_attributes
             |> processAttributes(~ignoreInterface, ~pos=ld_loc.loc_start)
           )
      | _ => ()
      };
      super.type_kind(self, typeKind);
    };
    let value_description =
        (
          self,
          {val_attributes, val_id, val_val} as value_description: Typedtree.value_description,
        ) => {
      val_attributes
      |> processAttributes(~ignoreInterface, ~pos=val_val.val_loc.loc_start);
      super.value_description(self, value_description);
    };
    {...super, type_kind, value_binding, value_description};
  };

  let structure = structure => {
    let ignoreInterface = ref(false);
    let collectExportLocations = collectExportLocations(~ignoreInterface);
    structure
    |> collectExportLocations.structure(collectExportLocations)
    |> ignore;
  };
  let signature = signature => {
    let ignoreInterface = ref(false);
    let collectExportLocations = collectExportLocations(~ignoreInterface);
    signature
    |> collectExportLocations.signature(collectExportLocations)
    |> ignore;
  };
};

type item = {
  pos: Lexing.position,
  path: string,
};

module WriteDeadAnnotations = {
  type annotation = {
    item,
    useColumn: bool,
  };
  type line = {
    mutable annotation: option(annotation),
    original: string,
  };

  let lineToString = ({original, annotation}) => {
    switch (annotation) {
    | None => original
    | Some({item: {pos, path}, useColumn}) =>
      let annotationStr = "[@" ++ deadAnnotation ++ " \"" ++ path ++ "\"] ";
      if (useColumn) {
        let col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol;
        let originalLen = String.length(original);
        assert(String.length(original) >= col);
        let original1 = String.sub(original, 0, col);
        let original2 = String.sub(original, col, originalLen - col);
        original1 ++ annotationStr ++ original2;
      } else {
        annotationStr ++ original;
      };
    };
  };

  let currentFile = ref("");
  let currentFileLines: ref(array(line)) = ref([||]);

  let readFile = fileName => {
    let channel = open_in(fileName);
    let lines = ref([]);
    let rec loop = () => {
      let line = {original: input_line(channel), annotation: None};
      lines := [line, ...lines^];
      loop();
    };
    try(loop()) {
    | End_of_file =>
      close_in(channel);
      lines^ |> List.rev |> Array.of_list;
    };
  };

  let writeFile = (fileName, lines) =>
    if (fileName != "" && write) {
      let channel = open_out(fileName);
      let lastLine = Array.length(lines);
      lines
      |> Array.iteri((n, line) => {
           output_string(channel, line |> lineToString);
           if (n < lastLine - 1) {
             output_char(channel, '\n');
           };
         });
      close_out(channel);
    };

  let onDeadItem = (~useColumn, {pos, path} as item) => {
    let fileName = pos.Lexing.pos_fname;
    if (fileName != currentFile^) {
      writeFile(currentFile^, currentFileLines^);
      currentFile := fileName;
      currentFileLines := readFile(fileName);
    };
    let indexInLines = pos.Lexing.pos_lnum - 1;
    let line = currentFileLines^[indexInLines];
    line.annotation = Some({item, useColumn});
    Printf.printf(
      "<-- line %d\n%s\n",
      pos.Lexing.pos_lnum,
      currentFileLines^[indexInLines] |> lineToString,
    );
  };

  let write = () => writeFile(currentFile^, currentFileLines^);
};

let report = (~useColumn, ~onDeadCode, decs: decs) => {
  let isValueDecs = decs === valueDecs;
  let dontReportDead = pos =>
    ProcessDeadAnnotations.isAnnotatedGentypeOrDead(pos);

  let folder = (items, {pos, path}) => {
    switch (pos |> PosHash.findSet(valueReferences)) {
    | referencesToLoc when !(pos |> dontReportDead) =>
      let liveReferences =
        referencesToLoc
        |> PosSet.filter(pos => !ProcessDeadAnnotations.isAnnotatedDead(pos));
      if (liveReferences |> PosSet.cardinal == 0) {
        if (transitive) {
          pos |> ProcessDeadAnnotations.posAnnotatedDead;
        };
        [{pos, path: pathWithoutHead(path)}, ...items];
      } else {
        if (verbose && isValueDecs) {
          let refsString =
            referencesToLoc
            |> PosSet.elements
            |> List.map(posToString(~printCol=true, ~shortFile=true))
            |> String.concat(", ");
          GenTypeCommon.logItem(
            "%s: %d references (%s)\n",
            path,
            referencesToLoc |> PosSet.cardinal,
            refsString,
          );
        };
        items;
      };
    | _ => items
    | exception Not_found => items
    };
  };

  if (isValueDecs && verbose) {
    GenTypeCommon.logItem("\nFile References\n\n");
    fileReferences
    |> FileHash.iter((file, files) =>
         GenTypeCommon.logItem(
           "%s -->> %s\n",
           file |> Filename.basename,
           files
           |> FileSet.elements
           |> List.map(Filename.basename)
           |> String.concat(", "),
         )
       );
  };

  let items =
    Hashtbl.fold((pos, path, items) => [{pos, path}, ...items], decs, []);
  let orderedFiles = Hashtbl.create(256);
  iterFilesFromRootsToLeaves(
    {
      let current = ref(0);
      fileName => {
        incr(current);
        Hashtbl.add(orderedFiles, fileName, current^);
      };
    },
  );
  let compareItemsUsingDependencies =
      (
        {
          path: path1,
          pos: {
            pos_fname: fname1,
            pos_lnum: lnum1,
            pos_bol: bol1,
            pos_cnum: cnum1,
          },
        },
        {
          path: path2,
          pos: {
            pos_fname: fname2,
            pos_lnum: lnum2,
            pos_bol: bol2,
            pos_cnum: cnum2,
          },
        },
      ) => {
    let findPosition = fn => Hashtbl.find(orderedFiles, fn);

    /* From the root of the file dependency DAG to the leaves.
       From the bottom of the file to the top */
    compare(
      (fname1 |> findPosition, lnum2, bol2, cnum2, path1),
      (fname2 |> findPosition, lnum1, bol1, cnum1, path2),
    );
  };
  items
  |> List.fast_sort(compareItemsUsingDependencies)  /* analyze in reverse order */
  |> List.fold_left(folder, [])
  |> List.iter(item => item |> onDeadCode(~useColumn));
};