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
