let posToString = (pos: Lexing.position) => {
  let file = pos.Lexing.pos_fname;
  let line = pos.Lexing.pos_lnum;
  let col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol;
  (file |> Filename.basename)
  ++ ":"
  ++ string_of_int(line)
  ++ ":"
  ++ string_of_int(col);
};

module ModulePath = {
  type t = list(string);
  let empty: t = [];

  let toString = (x: t) => x |> List.rev |> String.concat(".");
};

module Env = {
  module StringMap = Map.Make(String);
  type t = StringMap.t(Location.t);
  let empty: t = StringMap.empty;
  let addPath = (~path, ~loc, env) =>
    StringMap.add(path |> ModulePath.toString, loc, env);
  let findPath = (~modulePath, ~path, env: t) => {
    let pathName = Path.name(path);
    let rec loop = modulePath =>
      switch (
        StringMap.find_opt(
          [pathName, ...modulePath] |> ModulePath.toString,
          env,
        )
      ) {
      | None =>
        switch (modulePath) {
        | [] => None
        | [_, ...rest] => loop(rest)
        }
      | Some(_) as res => res
      };
    loop(modulePath);
  };

  let findExternalPath = (~path, env: t) => {
    let p =
      switch (Path.flatten(path)) {
      | `Contains_apply => assert(false)
      | `Ok(_id, p) => p
      };
    StringMap.find_opt(p |> ModulePath.toString, env);
  };
};

module ModulesTable = {
  type moduleInfo = {
    cmtFile: string,
    mutable exportedEnv: option(Env.t),
  };
  let table: Hashtbl.t(string, moduleInfo) = Hashtbl.create(10);

  let addModule = (~dir, ~file) => {
    let moduleName =
      file |> Filename.remove_extension |> String.capitalize_ascii;
    if (!Hashtbl.mem(table, moduleName)) {
      let cmtFile = Filename.concat(dir, file);
      Hashtbl.replace(table, moduleName, {cmtFile, exportedEnv: None});
      //Log_.item("moduleName:%s cmtFile:%s@.", moduleName, cmtFile);
    };
  };

  let rec processDir = (~subdirs, dir) =>
    if (dir |> Sys.file_exists && dir |> Sys.is_directory) {
      //Log_.item("module dir:%s@.", dir);
      dir
      |> Sys.readdir
      |> Array.iter(file =>
           if (Filename.check_suffix(file, ".cmt")) {
             addModule(~dir, ~file);
           } else if (subdirs) {
             processDir(~subdirs, Filename.concat(dir, file));
           }
         );
    };

  let populate = (~config) => {
    ["lib", "bs"]
    |> List.fold_left(Filename.concat, Config_.projectRoot^)
    |> processDir(~subdirs=true);

    ModuleResolver.readSourceDirs(~configSources=config.Config_.sources).pkgs
    |> Hashtbl.iter((_, dir) =>
         ["lib", "ocaml"]
         |> List.fold_left(Filename.concat, dir)
         |> processDir(~subdirs=false)
       );
  };

  let find = moduleName => Hashtbl.find_opt(table, moduleName);
};

let rec processPattern = (~currEnv, ~currModulePath, pat: Typedtree.pattern) =>
  switch (pat.pat_desc) {
  | Tpat_any => ()
  | Tpat_var(id, {loc}) =>
    let path = [Ident.name(id), ...currModulePath^];
    Log_.item(
      "Value binding:%s %s@.",
      path |> ModulePath.toString,
      loc.loc_start |> posToString,
    );
    currEnv := currEnv^ |> Env.addPath(~path, ~loc);
  | Tpat_alias(p, id, {loc}) =>
    let path = [Ident.name(id), ...currModulePath^];
    p |> processPattern(~currEnv, ~currModulePath);
    Log_.item(
      "Value binding alias:%s %s@.",
      path |> ModulePath.toString,
      loc.loc_start |> posToString,
    );
    currEnv := currEnv^ |> Env.addPath(~path, ~loc);
  | Tpat_constant(_) => assert(false)
  | Tpat_tuple(pats) =>
    pats |> List.iter(processPattern(~currEnv, ~currModulePath))
  | Tpat_construct(_loc, _cd, pats) =>
    pats |> List.iter(processPattern(~currEnv, ~currModulePath))
  | Tpat_variant(_) => assert(false)
  | Tpat_record(_) => assert(false)
  | Tpat_array(_) => assert(false)
  | Tpat_or(p1, _, _) => p1 |> processPattern(~currEnv, ~currModulePath)
  | Tpat_lazy(_) => assert(false)
  };

let processValueBindings =
    (
      ~currEnv,
      ~currModulePath,
      ~recFlag: Asttypes.rec_flag,
      ~self,
      valueBindings,
    ) => {
  switch (recFlag) {
  | Nonrecursive =>
    valueBindings
    |> List.iter((vb: Typedtree.value_binding) =>
         self.Tast_mapper.expr(self, vb.vb_expr) |> ignore
       );
    valueBindings
    |> List.iter((vb: Typedtree.value_binding) =>
         vb.vb_pat |> processPattern(~currEnv, ~currModulePath)
       );
  | Recursive =>
    valueBindings
    |> List.iter((vb: Typedtree.value_binding) =>
         vb.vb_pat |> processPattern(~currEnv, ~currModulePath)
       );
    valueBindings
    |> List.iter((vb: Typedtree.value_binding) =>
         self.Tast_mapper.expr(self, vb.vb_expr) |> ignore
       );
  };
};
let processExpr =
    (
      ~currEnv,
      ~currModulePath,
      ~readCmtExports,
      super,
      self,
      e: Typedtree.expression,
    ) => {
  switch (e.exp_desc) {
  | Texp_ident(path, {loc}, _) =>
    let foundLoc =
      switch (currEnv^ |> Env.findPath(~modulePath=currModulePath^, ~path)) {
      | Some(loc) => loc.loc_start |> posToString
      | None =>
        let moduleName = path |> Path.head |> Ident.name;
        switch (moduleName |> ModulesTable.find) {
        | None => "ModuleNotFound:" ++ moduleName
        | Some({cmtFile, exportedEnv} as moduleInfo) =>
          let env =
            switch (exportedEnv) {
            | None =>
              let env = cmtFile |> readCmtExports;
              moduleInfo.exportedEnv = Some(env);
              env;
            | Some(env) => env
            };
          switch (env |> Env.findExternalPath(~path)) {
          | None => "NotFound in " ++ (cmtFile |> Filename.basename)
          | Some(loc) => loc.loc_start |> posToString
          };
        };
      };
    Log_.item(
      "Ident:%s loc:%s ref:%s@.",
      Path.name(path),
      loc.loc_start |> posToString,
      foundLoc,
    );
    e;
  | Texp_let(recFlag, valueBindings, body) =>
    let oldEnv = currEnv^;
    valueBindings
    |> processValueBindings(~currEnv, ~currModulePath, ~recFlag, ~self);
    self.Tast_mapper.expr(self, body) |> ignore;
    currEnv := oldEnv;
    e;
  | _ => super.Tast_mapper.expr(self, e)
  };
};

let processCase =
    (
      ~currEnv,
      ~currModulePath,
      _super,
      self,
      {c_lhs, c_guard, c_rhs} as case: Typedtree.case,
    ) => {
  let oldEnv = currEnv^;
  c_lhs |> processPattern(~currEnv, ~currModulePath);
  switch (c_guard) {
  | None => ()
  | Some(guard) => self.Tast_mapper.expr(self, guard) |> ignore
  };
  self.Tast_mapper.expr(self, c_rhs) |> ignore;
  currEnv := oldEnv;
  case;
};

let processStructureItem =
    (~currEnv, ~currModulePath, super, self, si: Typedtree.structure_item) => {
  switch (si.str_desc) {
  | Tstr_value(recFlag, valueBindings) =>
    valueBindings
    |> processValueBindings(~currEnv, ~currModulePath, ~recFlag, ~self);
    si;
  | Tstr_module({mb_id}) =>
    let oldModulePath = currModulePath^;
    currModulePath := [Ident.name(mb_id), ...oldModulePath];
    super.Tast_mapper.structure_item(self, si) |> ignore;
    currModulePath := oldModulePath;
    si;
  | _ => super.Tast_mapper.structure_item(self, si)
  };
};

let processClassExpr =
    (~currEnv, ~currModulePath, super, self, ce: Typedtree.class_expr) => {
  switch (ce.cl_desc) {
  | Tcl_let(recFlag, valueBindings, ivars, cl) =>
    let oldEnv = currEnv^;
    valueBindings
    |> processValueBindings(~currEnv, ~currModulePath, ~recFlag, ~self);
    ivars
    |> List.iter(((_, _, e)) => super.Tast_mapper.expr(self, e) |> ignore);
    super.Tast_mapper.class_expr(self, cl) |> ignore;
    currEnv := oldEnv;
    ce;
  | _ => super.Tast_mapper.class_expr(self, ce)
  };
};

let traverseStructure = {
  let super = Tast_mapper.default;
  let currEnv = ref(Env.empty);
  let currModulePath = ref(ModulePath.empty);

  let case = (self, c) =>
    c |> processCase(~currEnv, ~currModulePath, super, self);
  let class_expr = (self, ce) =>
    ce |> processClassExpr(~currEnv, ~currModulePath, super, self);

  let readCmtExports = cmtFile => {
    Log_.item("@.");
    Log_.item("XXX reading %s@.", cmtFile);
    let currEnv = ref(Env.empty);
    let currModulePath = ref(ModulePath.empty);
    let inputCMT = GenTypeMain.readCmt(cmtFile);
    let {Cmt_format.cmt_annots} = inputCMT;
    switch (cmt_annots) {
    | Implementation(structure) =>
      let structure_item = (self, si) =>
        si |> processStructureItem(~currEnv, ~currModulePath, super, self);
      let mapper = Tast_mapper.{...super, structure_item};
      structure |> mapper.structure(mapper) |> ignore;
      Log_.item("XXX Done@.@.");
      currEnv^;
    | Interface(signature) => assert(false)
    | _ => assert(false)
    };
  };

  let expr = (self, e) =>
    e |> processExpr(~currEnv, ~currModulePath, ~readCmtExports, super, self);
  let structure_item = (self, si) =>
    si |> processStructureItem(~currEnv, ~currModulePath, super, self);

  Tast_mapper.{...super, case, class_expr, expr, structure_item};
};

let processCmtFile = (~config, cmtFile) => {
  Log_.item("FileInfo cmtFile:%s@.", cmtFile);
  ModulesTable.populate(~config);
  let inputCMT = GenTypeMain.readCmt(cmtFile);
  let {Cmt_format.cmt_annots} = inputCMT;
  switch (cmt_annots) {
  | Implementation(structure) =>
    structure |> traverseStructure.structure(traverseStructure) |> ignore
  | Interface(signature) => assert(false)
  | _ => assert(false)
  };
};
