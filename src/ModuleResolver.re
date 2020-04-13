module ModuleNameMap = Map.Make(ModuleName);

let (+++) = Filename.concat;

/* Read all the dirs from a library in node_modules */
let readBsDependenciesDirs = (~root) => {
  let dirs = ref([]);
  let rec findSubDirs = dir => {
    let absDir = dir == "" ? root : root +++ dir;
    if (Sys.file_exists(absDir) && Sys.is_directory(absDir)) {
      dirs := [dir, ...dirs^];
      absDir |> Sys.readdir |> Array.iter(d => findSubDirs(dir +++ d));
    };
  };
  findSubDirs("");
  dirs^;
};

type pkgs = {
  dirs: list(string),
  pkgs: Hashtbl.t(string, string),
};

type case =
  | Lowercase
  | Uppercase;

type resolver = {
  lazyFind:
    Lazy.t(
      (~useBsDependencies: bool, ModuleName.t) =>
      option((string, case, bool)),
    ),
};

let apply = (~resolver, ~useBsDependencies, moduleName) =>
  moduleName |> Lazy.force(resolver.lazyFind, ~useBsDependencies);
