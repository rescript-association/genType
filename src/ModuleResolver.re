let (+++) = Filename.concat;

let readDirsFromConfig = (~configSources) => {
  let dirs = ref([]);
  let root = Paths.projectRoot^;
  let (+++) = Filename.concat;

  let rec processDir = (~subdirs, dir) => {
    let absDir = dir == "" ? root : root +++ dir;
    if (Sys.file_exists(absDir) && Sys.is_directory(absDir)) {
      dirs := [dir, ...dirs^];
      if (subdirs) {
        absDir
        |> Sys.readdir
        |> Array.iter(d => processDir(~subdirs, dir +++ d));
      };
    };
  };

  let rec processSourceItem = (sourceItem: Ext_json_types.t) =>
    switch (sourceItem) {
    | Str({str}) => str |> processDir(~subdirs=false)
    | Obj({map}) =>
      switch (map |> String_map.find_opt("dir")) {
      | Some(Str({str})) =>
        let subdirs =
          switch (map |> String_map.find_opt("subdirs")) {
          | Some(True(_)) => true
          | Some(False(_)) => false
          | _ => false
          };
        str |> processDir(~subdirs);
      | _ => ()
      }
    | Arr({content}) => Array.iter(processSourceItem, content)
    | _ => ()
    };

  switch (configSources) {
  | Some(sourceItem) => processSourceItem(sourceItem)
  | None => ()
  };
  dirs^;
};

let readSourceDirs = (~configSources) => {
  let sourceDirs =
    ["lib", "bs", ".sourcedirs.json"]
    |> List.fold_left((+++), Paths.bsbProjectRoot^);
  let dirs = ref([]);

  let readDirs = json => {
    switch (json) {
    | Ext_json_types.Obj({map}) =>
      switch (map |> String_map.find_opt("dirs")) {
      | Some(Arr({content})) =>
        content
        |> Array.iter(x =>
             switch (x) {
             | Ext_json_types.Str({str}) => dirs := [str, ...dirs^]
             | _ => ()
             }
           );
        ();
      | _ => ()
      }
    | _ => ()
    };
  };

  if (sourceDirs |> Sys.file_exists) {
    try({
      let json = sourceDirs |> Ext_json_parse.parse_json_from_file;
      if (Paths.bsbProjectRoot^ != Paths.projectRoot^) {
        readDirs(json);
        dirs := readDirsFromConfig(~configSources);
      } else {
        readDirs(json);
      };
    }) {
    | _ => ()
    };
  } else {
    Log_.item("Warning: can't find source dirs: %s\n", sourceDirs);
    Log_.item("Types for cross-references will not be found by genType.\n");
    dirs := readDirsFromConfig(~configSources);
  };
  dirs^;
};
