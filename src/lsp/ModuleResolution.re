open Infix;

let rec resolveNodeModulePath = (~startPath, name) => {
  let path = startPath /+ "node_modules" /+ name;
  switch (startPath) {
  | "/" => Files.exists(path) ? Some(path) : None
  | _ =>
    Files.exists(path)
      ? Some(path)
      : resolveNodeModulePath(~startPath=Filename.dirname(startPath), name)
  };
};
