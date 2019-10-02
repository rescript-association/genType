include GenTypeCommon;

let active = Sys.getenv_opt("Global") != None;

let (+++) = Filename.concat;

let processCmt = (~libBsSourceDir, ~sourceDir, cmtFile) => {
  let extension = Filename.extension(cmtFile);
  let kind = extension == ".cmti" ? `Iface : `Implem;
  let sourceFile =
    Filename.chop_extension(projectRoot^ +++ sourceDir +++ cmtFile)
    ++ (kind == `Iface ? ".rei" : ".re");
  if (!Sys.file_exists(sourceFile)) {
    assert(false);
  };

  let cmtFilePath = Filename.concat(libBsSourceDir, cmtFile);
  DeadCode.load_file(~kind, ~sourceFile, cmtFilePath);
};

if (active) {
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

  DeadCode.run();
};