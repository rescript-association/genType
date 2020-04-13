let projectRoot = ref("");
let bsbProjectRoot = ref("");

let bsconfig = "bsconfig.json";
let rec findProjectRoot = (~dir) =>
  if (Sys.file_exists(Filename.concat(dir, bsconfig))) {
    dir;
  } else {
    let parent = dir |> Filename.dirname;
    if (parent == dir) {
      prerr_endline(
        "Error: cannot find project root containing " ++ bsconfig ++ ".",
      );
      assert(false);
    } else {
      findProjectRoot(~dir=parent);
    };
  };
let setProjectRoot = () => {
  projectRoot := findProjectRoot(~dir=Sys.getcwd());
  bsbProjectRoot :=
    (
      switch (Sys.getenv_opt("BSB_PROJECT_ROOT")) {
      | None => projectRoot^
      | Some(s) => s
      }
    );
};

/*
 * Handle namespaces in cmt files.
 * E.g. src/Module-Project.cmt becomes src/Module
 */
let handleNamespace = cmt => {
  let cutAfterDash = s =>
    switch (String.index(s, '-')) {
    | n => String.sub(s, 0, n)
    | exception Not_found => s
    };
  let noDir = Filename.basename(cmt) == cmt;
  if (noDir) {
    cmt |> Filename.chop_extension |> cutAfterDash;
  } else {
    let dir = cmt |> Filename.dirname;
    let base =
      cmt |> Filename.basename |> Filename.chop_extension |> cutAfterDash;
    Filename.concat(dir, base);
  };
};

let getModuleName = cmt =>
  cmt |> handleNamespace |> Filename.basename |> ModuleName.fromStringUnsafe;
