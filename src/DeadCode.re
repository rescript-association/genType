open DeadCommon;

let (+++) = Filename.concat;

let getModuleName = fn => fn |> Paths.getModuleName |> ModuleName.toString;

let rec getSignature = (~isfunc=false, moduleType: Types.module_type) =>
  switch (moduleType) {
  | Mty_signature(signature) => signature
  | Mty_functor(_, tOpt, _) when isfunc =>
    switch (tOpt) {
    | None => []
    | Some(moduleType) => getSignature(moduleType)
    }
  | Mty_functor(_, _, moduleType) => getSignature(moduleType)
  | _ => []
  };

let rec collectExportFromSignatureItem =
        (~implementationWithInterface, ~path, si: Types.signature_item) =>
  switch (si) {
  | Sig_value(id, {Types.val_loc, val_kind})
      when !val_loc.Location.loc_ghost && !implementationWithInterface =>
    let isPrimitive =
      switch (val_kind) {
      | Val_prim(_) => true
      | _ => false
      };
    if (!isPrimitive || analyzeExternals) {
      export(
        ~analysisKind=Value,
        ~decKind=Val,
        ~path,
        ~id,
        ~implementationWithInterface,
        ~loc=val_loc,
      );
    };
  | Sig_type(id, t, _) =>
    if (analyzeTypes^) {
      DeadType.collectTypeExport(
        ~implementationWithInterface,
        ~path=[id, ...path],
        t,
      );
    }
  | (
      Sig_module(id, {Types.md_type: moduleType}, _) |
      Sig_modtype(id, {Types.mtd_type: Some(moduleType)})
    ) as s =>
    let collect =
      switch (s) {
      | Sig_modtype(_) => false
      | _ => true
      };
    if (collect) {
      getSignature(moduleType)
      |> List.iter(
           collectExportFromSignatureItem(
             ~implementationWithInterface,
             ~path=[id, ...path],
           ),
         );
    };
  | _ => ()
  };

let processSignature =
    (~implementationWithInterface, signature: Types.signature) => {
  let module_id = Ident.create(String.capitalize_ascii(currentModuleName^));
  signature
  |> List.iter(sig_item =>
       collectExportFromSignatureItem(
         ~implementationWithInterface,
         ~path=[module_id],
         sig_item,
       )
     );
  lastPos := Lexing.dummy_pos;
};

let loadCmtFile = cmtFilePath => {
  lastPos := Lexing.dummy_pos;
  if (verbose) {
    Log_.item("Scanning %s\n", cmtFilePath);
  };

  let {Cmt_format.cmt_annots, cmt_sourcefile, cmt_value_dependencies} =
    Cmt_format.read_cmt(cmtFilePath);

  switch (cmt_sourcefile) {
  | None => ()

  | Some(sourceFile_) =>
    let sourceFile =
      if (Filename.check_suffix(sourceFile_, ".re.ml")) {
        Filename.chop_suffix(sourceFile_, ".ml");
      } else if (Filename.check_suffix(sourceFile_, ".re.mli")) {
        Filename.chop_suffix(sourceFile_, ".re.mli") ++ ".rei";
      } else {
        sourceFile_;
      };

    FileHash.addFile(fileReferences, sourceFile);
    currentSrc := sourceFile;
    currentModuleName := getModuleName(sourceFile);

    if (dce^) {
      switch (cmt_annots) {
      | Interface(signature) =>
        ProcessDeadAnnotations.signature(signature);
        processSignature(
          ~implementationWithInterface=false,
          signature.sig_type,
        );
      | Implementation(structure) =>
        let cmtiExists =
          Sys.file_exists(
            (cmtFilePath |> Filename.chop_extension) ++ ".cmti",
          );
        if (!cmtiExists) {
          ProcessDeadAnnotations.structure(structure);
        };
        DeadValue.processStructure(
          ~cmtiExists,
          cmt_value_dependencies,
          structure,
        );
        processSignature(
          ~implementationWithInterface=cmtiExists,
          structure.str_type,
        );
      | _ => ()
      };
    };
    if (analyzeTermination^) {
      switch (cmt_annots) {
      | Interface(signature) => ()
      | Implementation(structure) => Arnold.processStructure(structure)
      | _ => ()
      };
    };
  };
};

let reportResults = (~posInAliveWhitelist) => {
  let ppf = Format.std_formatter;
  let onItem = ({analysisKind, decKind, pos, path}) => {
    let loc = {Location.loc_start: pos, loc_end: pos, loc_ghost: false};
    let (name, message) =
      switch (decKind) {
      | Val => ("Warning Dead Value", "is never used")
      | Record => ("Warning Dead Type", "is a record label never used to read a value")
      | Variant => ("Warning Dead Type", "is a variant case which is never constructed")
      };
    Log_.info(~loc, ~name, (ppf, ()) =>
      Format.fprintf(ppf, "@{<info>%s@} %s", path, message)
    );
  };
  let onDeadCode = item => {
    item |> onItem;
    item |> WriteDeadAnnotations.onDeadItem(~ppf);
  };
  reportDead(~onDeadCode, ~posInAliveWhitelist);
  WriteDeadAnnotations.write();
};

let aliveWhitelist = ["DeadTestWhitelist"];
let aliveWhitelistTable = {
  let tbl = Hashtbl.create(1);
  List.iter(
    moduleName => Hashtbl.replace(tbl, moduleName, ()),
    aliveWhitelist,
  );
  tbl;
};

let posInAliveWhitelist = (pos: Lexing.position) => {
  let moduleName = pos.pos_fname |> getModuleName;
  Hashtbl.mem(aliveWhitelistTable, moduleName);
};

let runAnalysis = (~cmtRoot) => {
  if (dce^ || analyzeTermination^) {
    Log_.Color.setup();
  };
  switch (cmtRoot) {
  | Some(root) =>
    let rec walkSubDirs = dir => {
      let absDir = dir == "" ? root : root +++ dir;
      if (Sys.file_exists(absDir)) {
        if (Sys.is_directory(absDir)) {
          absDir |> Sys.readdir |> Array.iter(d => walkSubDirs(dir +++ d));
        } else if (Filename.check_suffix(absDir, ".cmt")
                   || Filename.check_suffix(absDir, ".cmti")) {
          absDir |> loadCmtFile;
        };
      };
    };
    walkSubDirs("");
    if (dce^) {
      reportResults(~posInAliveWhitelist);
    };
    if (analyzeTermination^) {
      Arnold.reportResults();
    };

  | None =>
    Paths.setProjectRoot();
    let lib_bs = {
      GenTypeCommon.projectRoot^ +++ "lib" +++ "bs";
    };

    let sourceDirs = ModuleResolver.readSourceDirs(~configSources=None);
    sourceDirs.dirs
    |> List.iter(sourceDir =>
         if (sourceDir |> whitelistSourceDir) {
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
           cmtFiles
           |> List.iter(cmtFile => {
                let cmtFilePath = Filename.concat(libBsSourceDir, cmtFile);
                cmtFilePath |> loadCmtFile;
              });
         }
       );

    if (dce^) {
      reportResults(~posInAliveWhitelist);
    };
    if (analyzeTermination^) {
      Arnold.reportResults();
    };
  };
};

let runTerminationAnalysis = (~cmtRoot) => {
  dce := false;
  analyzeTermination := true;
  runAnalysis(~cmtRoot);
};