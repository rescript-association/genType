open DeadCommon;

let (+++) = Filename.concat;

let getModuleName = fn => fn |> Paths.getModuleName |> ModuleName.toString;

let loadFile = (~sourceFile, cmtFilePath) => {
  lastPos := Lexing.dummy_pos;
  if (verbose) {
    GenTypeCommon.logItem("Scanning %s\n", cmtFilePath);
  };
  currentSrc := sourceFile;
  currentModuleName := getModuleName(sourceFile);

  let {Cmt_format.cmt_annots, cmt_value_dependencies} =
    Cmt_format.read_cmt(cmtFilePath);
  switch (cmt_annots) {
  | Interface(signature) =>
    ProcessDeadAnnotations.signature(signature);
    DeadValue.processSignature(cmtFilePath, signature.sig_type);
  | Implementation(structure) =>
    let cmtiExists =
      Sys.file_exists((cmtFilePath |> Filename.chop_extension) ++ ".cmti");
    if (!cmtiExists) {
      ProcessDeadAnnotations.structure(structure);
    };
    DeadValue.processStructure(
      ~cmtiExists,
      cmt_value_dependencies,
      structure,
    );
    if (!cmtiExists) {
      DeadValue.processSignature(cmtFilePath, structure.str_type);
    };
  | _ => ()
  };
};

let report = () => {
  let onItem = ({pos, path}) => {
    print_string(pos |> posToString);
    print_string(path);
    print_newline();
  };
  let onDeadCode = (~useColumn, item) => {
    onItem(item);
    item |> WriteDeadAnnotations.onDeadItem(~useColumn);
  };
  Printf.printf("\n%s:\n", "UNUSED EXPORTED VALUES");
  reportDead(~analysisKind=Value, ~useColumn=false, ~onDeadCode);
  Printf.printf("\n%s:\n", "UNUSED CONSTRUCTORS/RECORD FIELDS");
  reportDead(~analysisKind=Type, ~useColumn=true, ~onDeadCode);
  WriteDeadAnnotations.write();
};

let processCmt = (~libBsSourceDir, ~sourceDir, cmtFile) => {
  let extension = Filename.extension(cmtFile);
  let moduleName = cmtFile |> getModuleName;
  let sourceFile =
    (GenTypeCommon.projectRoot^ +++ sourceDir +++ moduleName)
    ++ (extension == ".cmti" ? ".rei" : ".re");
  if (!Sys.file_exists(sourceFile)) {
    GenTypeCommon.logItem("XXX sourceFile:%s\n", sourceFile);
    assert(false);
  };
  FileHash.addFile(fileReferences, sourceFile);

  let cmtFilePath = Filename.concat(libBsSourceDir, cmtFile);
  loadFile(~sourceFile, cmtFilePath);
};

let runAnalysis = () => {
  Paths.setProjectRoot();
  let lib_bs = {
    GenTypeCommon.projectRoot^ +++ "lib" +++ "bs";
  };

  let sourceDirs = ModuleResolver.readSourceDirs(~configSources=None);
  sourceDirs.dirs
  |> List.iter(sourceDir =>
       if (sourceDir |> whitelistSourceDir) {
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
       }
     );

  report();
};