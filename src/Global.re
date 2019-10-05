include GenTypeCommon;

let (+++) = Filename.concat;

/* Keep track of the location of values exported via genType */
module ExportedValues = {
  /* Locations exported to JS */
  let locationsAnnotatedWithGenType = DeadCommon.LocHash.create(1);
  let locationsAnnotatedDead = DeadCommon.LocHash.create(1);

  let dontReportDead = loc =>
    DeadCommon.LocHash.mem(locationsAnnotatedWithGenType, loc)
    || DeadCommon.LocHash.mem(locationsAnnotatedDead, loc);

  let locAnnotatedWithGenType = (loc: Lexing.position) => {
    DeadCommon.LocHash.replace(locationsAnnotatedWithGenType, loc, ());
  };

  let locAnnotatedDead = (loc: Lexing.position) => {
    DeadCommon.LocHash.replace(locationsAnnotatedDead, loc, ());
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
        if (vb_attributes |> Annotation.hasGenTypeAnnotation(~ignoreInterface)) {
          pLoc.loc.loc_start |> locAnnotatedWithGenType;
        };
        if (vb_attributes
            |> Annotation.getAttributePayload(
                 (==)(DeadCommon.deadAnnotation),
               )
            != None) {
          pLoc.loc.loc_start |> locAnnotatedDead;
        };

      | _ => ()
      };
      super.value_binding(self, value_binding);
    };
    let value_description =
        (
          self,
          {val_attributes, val_id, val_loc} as value_description: Typedtree.value_description,
        ) => {
      if (val_attributes |> Annotation.hasGenTypeAnnotation(~ignoreInterface)) {
        val_loc.loc_start |> locAnnotatedWithGenType;
      };
      super.value_description(self, value_description);
    };
    {...super, value_binding, value_description};
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

let processCmt = (~libBsSourceDir, ~sourceDir, cmtFile) => {
  let extension = Filename.extension(cmtFile);
  let moduleName = cmtFile |> DeadCommon.getModuleName;
  let sourceFile =
    (projectRoot^ +++ sourceDir +++ moduleName)
    ++ (extension == ".cmti" ? ".rei" : ".re");
  if (!Sys.file_exists(sourceFile)) {
    GenTypeCommon.logItem("XXX sourceFile:%s\n", sourceFile);
    assert(false);
  };

  let cmtFilePath = Filename.concat(libBsSourceDir, cmtFile);
  DeadCode.load_file(
    ~exportedValuesSignature=ExportedValues.signature,
    ~exportedValuesStructure=ExportedValues.structure,
    ~sourceFile,
    cmtFilePath,
  );
};

let readFile = fileName => {
  let channel = open_in(fileName);
  let lines = ref([]);
  let rec loop = () => {
    let line = input_line(channel);
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
  if (fileName != "" && DeadCommon.write) {
    let channel = open_out(fileName);
    let lastLine = Array.length(lines);
    lines
    |> Array.iteri((n, line) => {
         output_string(channel, line);
         if (n < lastLine - 1) {
           output_char(channel, '\n');
         };
       });
  };

let runAnalysis = () => {
  Paths.setProjectRoot();
  let lib_bs = {
    projectRoot^ +++ "lib" +++ "bs";
  };

  ModuleResolver.readSourceDirs()
  |> List.iter(sourceDir => {
       let libBsSourceDir = Filename.concat(lib_bs, sourceDir);
       let files =
         switch (Sys.readdir(libBsSourceDir) |> Array.to_list) {
         | files => files
         | exception (Sys_error(_)) => []
         };
       let cmtFiles =
         files
         |> List.filter(x =>
              Filename.check_suffix(x, ".cmt")
              || Filename.check_suffix(x, ".cmti")
            );
       cmtFiles |> List.iter(processCmt(~libBsSourceDir, ~sourceDir));
     });

  let currentFile = ref("");
  let currentFileLines = ref([||]);
  let onUnusedValue = (loc, path) => {
    let fileName = loc.Lexing.pos_fname;
    if (fileName != currentFile^) {
      writeFile(currentFile^, currentFileLines^);
      currentFile := fileName;
      currentFileLines := readFile(fileName);
    };
    let indexInLines = loc.Lexing.pos_lnum - 1;
    currentFileLines^[indexInLines] =
      "[@"
      ++ DeadCommon.deadAnnotation
      ++ " \""
      ++ path
      ++ "\"] "
      ++ currentFileLines^[indexInLines];
    Printf.printf(
      "<-- line %d\n%s\n",
      loc.Lexing.pos_lnum,
      currentFileLines^[indexInLines],
    );
  };
  DeadCode.report(
    ~dontReportDead=ExportedValues.dontReportDead,
    ~onUnusedValue,
  );
  writeFile(currentFile^, currentFileLines^);
};