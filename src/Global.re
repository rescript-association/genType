include GenTypeCommon;

let active = true;

if (active) {
  logItem("Global!\n");

  let projectRoot = Paths.findProjectRoot(~dir=Sys.getcwd());
  let lib_bs = {
    let (++) = Filename.concat;
    projectRoot ++ "lib" ++ "bs";
  };
  logItem("lib_bs: %s\n", lib_bs);

  let sourceDirs =
    ModuleResolver.readSourceDirs() |> List.map(Filename.concat(lib_bs));
  sourceDirs
  |> List.iter(sourceDir => {
       let files =
         switch (Sys.readdir(sourceDir) |> Array.to_list) {
         | files => files
         | exception (Sys_error(_)) => []
         };
       let cmtFiles =
         files |> List.filter(x => Filename.extension(x) == ".cmt");
       cmtFiles |> List.iter(cmtFile => logItem("cmtFile: %s\n", cmtFile));
     });
};