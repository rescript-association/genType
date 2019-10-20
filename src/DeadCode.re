open DeadCommon;

let (+++) = Filename.concat;

let loadFile = (~sourceFile, cmtFilePath) => {
  lastPos := Lexing.dummy_pos;
  if (verbose) {
    GenTypeCommon.logItem("Scanning %s\n", cmtFilePath);
  };
  currentSrc := sourceFile;
  let {Cmt_format.cmt_annots, cmt_value_dependencies} =
    Cmt_format.read_cmt(cmtFilePath);
  switch (cmt_annots) {
  | Interface(signature) =>
    ProcessAnnotations.signature(signature);
    DeadValue.processSignature(cmtFilePath, signature.sig_type);
  | Implementation(structure) =>
    let cmtiExists =
      Sys.file_exists((cmtFilePath |> Filename.chop_extension) ++ ".cmti");
    if (!cmtiExists) {
      ProcessAnnotations.structure(structure);
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

let report = (~onDeadValue) => {
  let onItem = ({pos, path}) => {
    print_string(pos |> posToString);
    print_string(path);
    print_newline();
  };
  let onDeadValue = item => {
    onItem(item);
    onDeadValue(item);
  };
  Printf.printf("\n%s:\n", "UNUSED EXPORTED VALUES");
  valueDecs |> report(~onItem=onDeadValue);
  Printf.printf("\n%s:\n", "UNUSED CONSTRUCTORS/RECORD FIELDS");
  typeDecs |> report(~onItem);
};

let processCmt = (~libBsSourceDir, ~sourceDir, cmtFile) => {
  let extension = Filename.extension(cmtFile);
  let moduleName = cmtFile |> DeadCommon.getModuleName;
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
    GenTypeCommon.projectRoot^ +++ "lib" +++ "bs";
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
  let onDeadValue = (DeadCommon.{pos, path}) => {
    let fileName = pos.Lexing.pos_fname;
    if (fileName != currentFile^) {
      writeFile(currentFile^, currentFileLines^);
      currentFile := fileName;
      currentFileLines := readFile(fileName);
    };
    let indexInLines = pos.Lexing.pos_lnum - 1;
    currentFileLines^[indexInLines] =
      "[@"
      ++ DeadCommon.deadAnnotation
      ++ " \""
      ++ path
      ++ "\"] "
      ++ currentFileLines^[indexInLines];
    Printf.printf(
      "<-- line %d\n%s\n",
      pos.Lexing.pos_lnum,
      currentFileLines^[indexInLines],
    );
  };
  report(~onDeadValue);
  writeFile(currentFile^, currentFileLines^);
};