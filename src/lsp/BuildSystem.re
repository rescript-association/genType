let namespacedName = (namespace, name) =>
  switch (namespace) {
  | None => name
  | Some(namespace) => name ++ "-" ++ namespace
  };

open Infix;

let nodePlatform =
  lazy(
    switch (Sys.os_type) {
    | "Unix" =>
      switch (input_line(Unix.open_process_in("uname -s"))) {
      | "Darwin" => "darwin"
      | "Linux" => "linux"
      | "FreeBSD" => "freebsd"
      | s => invalid_arg(s ++ ": unsupported os_type")
      }
    | "Win32" => "win32"
    | s => invalid_arg(s ++ ": unsupported os_type")
    }
  );

let getBsPlatformDir = rootPath => {
  let result =
    ModuleResolution.resolveNodeModulePath(
      ~startPath=rootPath,
      "bs-platform",
    );
  switch (result) {
  | Some(path) => Ok(path)
  | None =>
    let message = "bs-platform could not be found";
    Log.log(message);
    Error(message);
  };
};

let getCompiledBase = root => {
  Files.ifExists(root /+ "lib" /+ "bs");
};
let getStdlib = base => {
  let%try_wrap bsPlatformDir = getBsPlatformDir(base);
  bsPlatformDir /+ "lib" /+ "ocaml";
};

let getCompiler = rootPath => {
  let%try_wrap bsPlatformDir = getBsPlatformDir(rootPath);
  switch (Files.ifExists(bsPlatformDir /+ "lib" /+ "bsc.exe")) {
  | Some(x) => x
  | None => bsPlatformDir /+ Lazy.force(nodePlatform) /+ "bsc.exe"
  };
};

let getRefmt = rootPath => {
  let%try_wrap bsPlatformDir = getBsPlatformDir(rootPath);
  switch (Files.ifExists(bsPlatformDir /+ "lib" /+ "refmt.exe")) {
  | Some(x) => x
  | None =>
    switch (
      Files.ifExists(
        bsPlatformDir /+ Lazy.force(nodePlatform) /+ "refmt.exe",
      )
    ) {
    | Some(x) => x
    | None => bsPlatformDir /+ "lib" /+ "refmt3.exe"
    }
  };
};

let hiddenLocation = rootPath => {
  Ok(rootPath /+ "node_modules" /+ ".lsp");
};
