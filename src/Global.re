include GenTypeCommon;

let (+++) = Filename.concat;

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
    ~processAnnotationsSignature=DeadCode.ProcessAnnotations.signature,
    ~processAnnotationsStructure=DeadCode.ProcessAnnotations.structure,
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
  let onUnusedValue = (DeadCommon.{pos, path}) => {
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
  DeadCode.report(
    ~dontReportDead=DeadCode.ProcessAnnotations.isAnnotatedGentypeOrDead,
    ~onUnusedValue,
  );
  writeFile(currentFile^, currentFileLines^);
};