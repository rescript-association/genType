/* Adapted from https://github.com/LexiFi/dead_code_analyzer */

// Turn on the main dead code analysis
let dce = ref(true);

// Turn on type analysis
let analyzeTypes = ref(true);

let analyzeTermination = ref(false);

let analyzeExternals = false;

let transitive = true;

let verbose = Sys.getenv_opt("Debug") != None;

let checkPrefix = prefix_ => {
  let prefix =
    GenTypeCommon.projectRoot^ == ""
      ? prefix_ : Filename.concat(GenTypeCommon.projectRoot^, prefix_);
  let prefixLen = prefix |> String.length;
  sourceDir =>
    String.length(sourceDir) >= prefixLen
    && String.sub(sourceDir, 0, prefixLen) == prefix;
};

// Whitelist=prefix only report on source dirs with the given prefix
let whitelistSourceDir =
  lazy(
    {
      switch (Sys.getenv_opt("Whitelist")) {
      | None => (_sourceDir => true)
      | Some(prefix) => checkPrefix(prefix)
      };
    }
  );

let posInWhitelist = (pos: Lexing.position) => {
  pos.pos_fname |> Lazy.force(whitelistSourceDir);
};

// Blacklist=prefix don't report on source dirs with the given prefix
let blacklistSourceDir =
  lazy(
    {
      switch (Sys.getenv_opt("Blacklist")) {
      | None => (_sourceDir => false)
      | Some(prefix) => checkPrefix(prefix)
      };
    }
  );

let posInBlacklist = (pos: Lexing.position) => {
  pos.pos_fname |> Lazy.force(blacklistSourceDir);
};

let write = Sys.getenv_opt("Write") != None;

let deadAnnotation = "dead";
let liveAnnotation = "live";

type declKind =
  | RecordLabel
  | VariantCase
  | Value;

/* Location printer: `filename:line: ' */
let posToString = (~printCol=true, ~shortFile=true, pos: Lexing.position) => {
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

  let mergeSet = (~isType, ~from, ~to_, table) => {
    let set1 = findSet(table, to_);
    let set2 = findSet(table, from);
    let setUnion = PosSet.union(set1, set2);
    replace(table, to_, setUnion);
    if (verbose) {
      Log_.item(
        "%smergeSet %s --> %s\n",
        isType ? "[type] " : "",
        from |> posToString,
        to_ |> posToString,
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

type decls = Hashtbl.t(Lexing.position, (string, declKind));
let decls: decls = Hashtbl.create(256); /* all exported declarations */

let valueReferences: PosHash.t(PosSet.t) = PosHash.create(256); /* all value references */
let typeReferences: PosHash.t(PosSet.t) = PosHash.create(256); /* all type references */

let fileReferences: FileHash.t(FileSet.t) = FileHash.create(256); /* references across files */
let implementationFilesWithInterface: FileHash.t(unit) =
  FileHash.create(256); /* implementation files that have an interface */

let fields: Hashtbl.t(string, Lexing.position) = (
  Hashtbl.create(256): Hashtbl.t(string, Lexing.position)
); /* link from fields (record/variant) paths and locations */

let lastPos = ref(Lexing.dummy_pos); /* helper to diagnose occurrences of Location.none in the typedtree */
let currentSrc = ref("");
let currentModuleName = ref("");
let currentBindingPos = ref(Lexing.dummy_pos);

let none_ = "_none_";
let include_ = "*include*";

/********   HELPERS   ********/

let addValueReference = (~addFileReference, posDeclaration, posUsage) => {
  let posUsage =
    !transitive || currentBindingPos^ == Lexing.dummy_pos
      ? posUsage : currentBindingPos^;
  if (verbose) {
    Log_.item(
      "addValueReference %s --> %s\n",
      posUsage |> posToString,
      posDeclaration |> posToString,
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
  // Process any remaining items in case of circular references
  referencesByNumber
  |> Hashtbl.iter((_num, set) =>
       if (FileSet.is_empty(set)) {
         ();
       } else {
         set
         |> FileSet.iter(fileName => {
              let pos = {...Lexing.dummy_pos, pos_fname: fileName};
              let loc = {...Location.none, loc_start: pos, loc_end: pos};
              Log_.info(~loc, ~name="Warning Dead Analysis Cycle", (ppf, ()) =>
                Format.fprintf(
                  ppf,
                  "Results for %s could be inaccurate because of circular references",
                  fileName,
                )
              );
              iterFun(fileName);
            });
       }
     );
};

/********   PROCESSING  ********/

let export = (~declKind, ~path, ~id, ~implementationWithInterface, ~loc) => {
  let path = String.concat(".", List.rev_map(Ident.name, [id, ...path]));
  let pos = loc.Location.loc_start;

  /* a .cmi file can contain locations from other files.
       For instance:
           module M : Set.S with type elt = int
       will create value definitions whose location is in set.mli
     */
  if (!loc.loc_ghost
      && (currentSrc^ == pos.pos_fname || currentModuleName^ === include_)) {
    if (verbose) {
      Log_.item(
        "%sexport %s %s\n",
        declKind != Value ? "[type] " : "",
        id.name,
        pos |> posToString,
      );
    };

    if (implementationWithInterface && declKind != Value) {
      if (!FileHash.mem(implementationFilesWithInterface, pos.pos_fname)) {
        FileHash.replace(implementationFilesWithInterface, pos.pos_fname, ());
      };
    };

    Hashtbl.add(decls, pos, (path, declKind));
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

/* Keep track of the location of values annotated @genType or @dead */
module ProcessDeadAnnotations = {
  type annotatedAs =
    | GenType
    | Dead
    | Live;

  let positionsAnnotated = PosHash.create(1);

  let isAnnotatedDead = pos =>
    PosHash.find_opt(positionsAnnotated, pos) == Some(Dead);

  let isAnnotatedLive = pos =>
    PosHash.find_opt(positionsAnnotated, pos) == Some(Live);

  let isAnnotatedGenType = pos =>
    PosHash.find_opt(positionsAnnotated, pos) == Some(GenType);

  let isAnnotatedGenTypeOrDead = pos =>
    switch (PosHash.find_opt(positionsAnnotated, pos)) {
    | Some(Dead | GenType) => true
    | Some(Live)
    | None => false
    };

  let annotateGenType = (pos: Lexing.position) => {
    PosHash.replace(positionsAnnotated, pos, GenType);
  };

  let annotateDead = (pos: Lexing.position) => {
    PosHash.replace(positionsAnnotated, pos, Dead);
  };

  let annotateLive = (pos: Lexing.position) => {
    PosHash.replace(positionsAnnotated, pos, Live);
  };

  let processAttributes = (~pos, attributes) => {
    if (attributes
        |> Annotation.hasGenTypeAnnotation(~ignoreInterface=ref(false))) {
      pos |> annotateGenType;
    };
    if (attributes
        |> Annotation.getAttributePayload((==)(deadAnnotation)) != None) {
      pos |> annotateDead;
    } else if (attributes
               |> Annotation.getAttributePayload((==)(liveAnnotation))
               != None) {
      pos |> annotateLive;
    };
  };

  let collectExportLocations = () => {
    let super = Tast_mapper.default;
    let value_binding =
        (
          self,
          {vb_attributes, vb_pat} as value_binding: Typedtree.value_binding,
        ) => {
      switch (vb_pat.pat_desc) {
      | Tpat_var(_id, pLoc) =>
        vb_attributes |> processAttributes(~pos=pLoc.loc.loc_start)

      | _ => ()
      };
      super.value_binding(self, value_binding);
    };
    let type_kind = (self, typeKind: Typedtree.type_kind) => {
      switch (typeKind) {
      | Ttype_record(labelDeclarations) =>
        labelDeclarations
        |> List.iter(({ld_attributes, ld_loc}: Typedtree.label_declaration) =>
             ld_attributes |> processAttributes(~pos=ld_loc.loc_start)
           )
      | Ttype_variant(constructorDeclarations) =>
        constructorDeclarations
        |> List.iter(
             ({cd_attributes, cd_loc}: Typedtree.constructor_declaration) =>
             cd_attributes |> processAttributes(~pos=cd_loc.loc_start)
           )
      | _ => ()
      };
      super.type_kind(self, typeKind);
    };
    let value_description =
        (
          self,
          {val_attributes, val_val} as value_description: Typedtree.value_description,
        ) => {
      val_attributes |> processAttributes(~pos=val_val.val_loc.loc_start);
      super.value_description(self, value_description);
    };
    {...super, type_kind, value_binding, value_description};
  };

  let structure = structure => {
    let collectExportLocations = collectExportLocations();
    structure
    |> collectExportLocations.structure(collectExportLocations)
    |> ignore;
  };
  let signature = signature => {
    let collectExportLocations = collectExportLocations();
    signature
    |> collectExportLocations.signature(collectExportLocations)
    |> ignore;
  };
};

type item = {
  declKind,
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
    | Some({item: {declKind, pos, path}, useColumn}) =>
      let isReason =
        Filename.check_suffix(pos.pos_fname, ".re")
        || Filename.check_suffix(pos.pos_fname, ".rei");
      let annotationStr =
        "["
        ++ (isReason || declKind != Value ? "@" : "@@")
        ++ deadAnnotation
        ++ " \""
        ++ path
        ++ "\"] ";
      if (!isReason) {
        original ++ " (* " ++ annotationStr ++ "*)";
      } else if (useColumn) {
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

  let onDeadItem = (~ppf, {declKind, pos} as item) => {
    let useColumn = declKind != Value;
    let fileName = pos.Lexing.pos_fname;
    if (Sys.file_exists(fileName)) {
      if (fileName != currentFile^) {
        writeFile(currentFile^, currentFileLines^);
        currentFile := fileName;
        currentFileLines := readFile(fileName);
      };
      let indexInLines = pos.Lexing.pos_lnum - 1;
      if (indexInLines < Array.length(currentFileLines^)) {
        let line = currentFileLines^[indexInLines];
        line.annotation = Some({item, useColumn});
        Format.fprintf(
          ppf,
          "  <-- line %d@.  %s@.",
          pos.Lexing.pos_lnum,
          line |> lineToString,
        );
      } else {
        Format.fprintf(
          ppf,
          "  <-- Can't find line %d@.",
          pos.Lexing.pos_lnum,
        );
      };
    } else {
      Format.fprintf(ppf, "  <-- can't find file@.");
    };
  };

  let write = () => writeFile(currentFile^, currentFileLines^);
};

let reportDead = (~onDeadCode) => {
  let dontReportDead = pos =>
    ProcessDeadAnnotations.isAnnotatedGenTypeOrDead(pos);

  let folder = (items, {declKind, pos, path} as item) => {
    switch (
      pos
      |> PosHash.findSet(declKind == Value ? valueReferences : typeReferences)
    ) {
    | referencesToLoc when !(pos |> dontReportDead) =>
      let liveReferences =
        referencesToLoc
        |> PosSet.filter(pos => !ProcessDeadAnnotations.isAnnotatedDead(pos));
      if (liveReferences
          |> PosSet.cardinal == 0
          && !ProcessDeadAnnotations.isAnnotatedLive(pos)
          && posInWhitelist(pos)
          && !posInBlacklist(pos)) {
        if (transitive) {
          pos |> ProcessDeadAnnotations.annotateDead;
        };
        if (declKind != Value
            && FileHash.mem(implementationFilesWithInterface, pos.pos_fname)) {
          // Don't report types dead in an implementation, if there's an interface.
          items;
        } else {
          [{...item, path: pathWithoutHead(path)}, ...items];
        };
      } else {
        if (verbose) {
          let refsString =
            referencesToLoc
            |> PosSet.elements
            |> List.map(posToString)
            |> String.concat(", ");
          Log_.item(
            "%s%s: %d references (%s)\n",
            declKind != Value ? "[type] " : "",
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

  if (verbose) {
    Log_.item("\nFile References\n\n");
    fileReferences
    |> FileHash.iter((file, files) =>
         Log_.item(
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
    Hashtbl.fold(
      (pos, (path, declKind), items) => [{declKind, pos, path}, ...items],
      decls,
      [],
    );

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
          declKind: kind1,
          path: path1,
          pos: {
            pos_fname: fname1,
            pos_lnum: lnum1,
            pos_bol: bol1,
            pos_cnum: cnum1,
          },
        },
        {
          declKind: kind2,
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

    let rec checkSub = (s1, s2, n) =>
      n <= 0 || s1.[n] == s2.[n] && checkSub(s1, s2, n - 1);
    let isImplementationOf = (s1, s2) => {
      let n1 = String.length(s1)
      and n2 = String.length(s2);
      n2 == n1 + 1 && checkSub(s1, s2, n1 - 1);
    };

    /* From the root of the file dependency DAG to the leaves.
       From the bottom of the file to the top.
       But implementation before interface. */
    let (position1, position2) =
      isImplementationOf(fname1, fname2)
        ? (1, 0)
        : isImplementationOf(fname2, fname1)
            ? (0, 1) : (fname1 |> findPosition, fname2 |> findPosition);
    compare(
      (position1, lnum2, bol2, cnum2, kind1, path1, kind1),
      (position2, lnum1, bol1, cnum1, kind2, path2, kind2),
    );
  };

  items
  |> List.fast_sort(compareItemsUsingDependencies)  /* analyze in reverse order */
  |> List.fold_left(folder, [])
  |> List.iter(item => item |> onDeadCode);
};