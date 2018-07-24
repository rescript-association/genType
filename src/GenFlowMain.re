/***
 * Copyright 2004-present Facebook. All Rights Reserved.
 */

module StringSet = Set.Make(String);

open GenFlowCommon;

/*
 * TODO:
 * Change the name of types/variables "conversion". It is not a conversion, it
 * representation a conversion task.
 */
/*
 * TODO: Inject a Flow opaque type for this variant type. Also inject a .match
 * export binding for the type.
 */

/*
  When reading the data structures: There are structures from the `Typedtree`
  module which is the typed AST, and that AST references types from the module
  `Types` which represent the result of type checking.

  - `Typedtree` usually has record fields of the form `typ_x`
  - `Typedtree` usually has variants of the form Ttype_foo
  - `Types` usually has record fields of the form `type_x`
  - `Types` usually has variants of the form Type_foo
  - types (not fields or variant names) defined in both `Typedtree` and
  `Types` begin with the prefix `type_foo`

  There is a lot of redundancy between the two trees.

  Typedtree Module:                                Types Module:
  ----------------------                           ----------------------
  type_declaration = {                          -->type type_declaration = {
                                               /     type_params: type_expr list,
    typ_id: Ident.t,                          /      type_arity: int,
    typ_name: string loc,                    /       type_kind: type_kind,
    typ_params: (core_type * variance) list,/        type_private: private_flag,
    typ_type: Types.type_declaration,  ----/         type_manifest: type_expr option,
    typ_cstrs:(core_type*core_type*Location.t)list   type_variance: Variance.t list,
    typ_kind: type_kind,                             (* covariant, contravariant, weakly contravariant, injective *)
    typ_private: private_flag,                       type_is_newtype: bool,
    typ_manifest: core_type option,                  type_expansion_scope: int option,
    typ_loc: Location.t,                             type_loc: Location.t,
    typ_attributes: attributes                       type_attributes: Parsetree.attributes,
    typ_attributes: attributes                       type_immediate: bool, (* true iff type should not be a pointer *)
  }                                                  type_unboxed: unboxed_status,
                                                   }

  Typedtree Module:                                Types Module:
  ----------------------                           ----------------------
  type type_kind =                                 type type_kind =
  | Ttype_abstract                                 | Type_abstract
  | Ttype_variant of constructor_declaration list  | Type_record of label_declaration list * record_representation
  | Ttype_record of label_declaration list         | Type_variant of constructor_declaration list
  | Ttype_open                                     | Type_open

  Typedtree Module:                                Types Module:
  ----------------------                           ----------------------
  type constructor_declaration = {                 type constructor_declaration = {
    cd_id: Ident.t,                                   cd_id: Ident.t,
    cd_name: string loc,                              cd_args: constructor_arguments,
    cd_args: constructor_arguments,                   cd_res: type_expr option,
    cd_res: core_type option,                         cd_loc: Location.t,
    cd_loc: Location.t,                               cd_attributes: Parsetree.attributes,
    cd_attributes: attributes                      }
  }

  type constructor_arguments =                     type constructor_arguments =
  | Cstr_tuple of core_type list                    | Cstr_tuple of type_expr list
  | Cstr_record of label_declaration list           | Cstr_record of label_declaration list


                        This pointer is mutated
  Typedtree Module:    with the result of type     Types Module:
  -------------        checking once complete!     -------------
  type core_type = {                          ---> type type_expr = {
    mutable ctyp_desc : core_type_desc;      /       mutable desc: type_desc;
    mutable ctyp_type : type_expr; ---------/        mutable level: int;
    ctyp_env : Env.t;                                mutable scope: int option;
    ctyp_loc : Location.t;                           id: int
    ctyp_attributes: attributes;                   }
  }

  type core_type_desc =                            type_desc =
  | Ttyp_any                                        | Tvar of string option
  | Ttyp_var of string                              | Tarrow of arg_label * type_expr * type_expr * commutable
  | Ttyp_arrow of arg_label*core_type*core_type     | Ttuple of type_expr list
  | Ttyp_tuple of core_type list                    | Tconstr of Path.t * type_expr list * abbrev_memo ref
  | Ttyp_constr of                                  | Tobject of type_expr * (Path.t * type_expr list) option ref
    Path.t*Longident.t loc*core_type list           | Tfield of string * field_kind * type_expr * type_expr
  | Ttyp_object of object_field list * closed_flag  | Tnil
  | Ttyp_class of                                   | Tlink of type_expr
     Path.t * Longident.t loc * core_type list      | Tsubst of type_expr         (* for copying *)
  | Ttyp_alias of core_type * string                | Tvariant of row_desc
  | Ttyp_variant of                                 | Tunivar of string option
     row_field list*closed_flag*label list option   | Tpoly of type_expr * type_expr list
  | Ttyp_poly of string list * core_type
  | Ttyp_package of package_type

  Typedtree.type_declaration will be something like: {
    type_type: {
      type_params: ...
      type_kind: type_kind =
         | Type_abstract
         | Type_record of label_declaration list  * record_representation
         | Type_variant of constructor_declaration list
         | Type_open
      type_manifest: Some({
        mutable desc: type_desc =
           | Tvar of string option
           | Tarrow of arg_label * type_expr * type_expr * commutable
           | Ttuple of type_expr list
           | Tconstr of Path.t * type_expr list * abbrev_memo ref
           | Tobject of type_expr * (Path.t * type_expr list) option ref
           | Tfield of string * field_kind * type_expr * type_expr
           | Tnil
           | Tlink of type_expr
           | Tsubst of type_expr         (* for copying *)
           | Tvariant of row_desc
           | Tunivar of string option
           | Tpoly of type_expr * type_expr list
      }),
      type_arity: int;
      type_private: private_flag;
      type_variance: Variance.t list;
      type_is_newtype: bool;
      type_expansion_scope: int option;
      type_loc: Location.t;
      type_attributes: Parsetree.attributes;
      type_immediate: bool; (* true iff type should not be a pointer *)
      type_unboxed: unboxed_status;
    }
  }
 */

let nullyToOptionalConverter = nullyValue => {};

let pp = Printf.fprintf;

let typedItemToCodeItems = (~inputModuleName, typedItem) => {
  let (listListDeps, listListItems) =
    switch (typedItem) {
    | {Typedtree.str_desc: Typedtree.Tstr_type(typeDeclarations)} =>
      typeDeclarations
      |> List.map(CodeItem.fromTypeDecl(~inputModuleName))
      |> List.split
    | {Typedtree.str_desc: Tstr_value(loc, valueBindings)} =>
      valueBindings
      |> List.map(CodeItem.fromValueBinding(~inputModuleName))
      |> List.split
    | _ => ([], [])
    /* TODO: Support mapping of variant type definitions. */
    };
  (List.concat(listListDeps), List.concat(listListItems));
};

let cmtToCodeItems =
    (~modulesMap, ~globalModuleName, inputCMT): list(CodeItem.t) => {
  let {Cmt_format.cmt_annots} = inputCMT;
  switch (cmt_annots) {
  | Implementation(structure) =>
    let typedItems = structure.Typedtree.str_items;
    let (deps, codeItems) =
      List.fold_left(
        ((curDeps, curParseItems), nextTypedItem) => {
          let (nextDeps, nextCodeItems) =
            nextTypedItem
            |> typedItemToCodeItems(~inputModuleName=globalModuleName);
          (nextDeps @ curDeps, nextCodeItems @ curParseItems);
        },
        ([], []),
        typedItems,
      );
    let imports = CodeItem.fromDependencies(modulesMap, deps);
    List.append(imports, codeItems);
  | _ => []
  };
};

let log = Printf.printf;
let logItem = x => {
  log("  > ");
  log(x);
};

module GeneratedReFiles = {
  type t = {
    filesOnDisk: StringSet.t,
    mutable filesToWrite: StringSet.t,
  };

  type fileAction =
    | NoMatch /* No @genFlow annotation found. */
    | Replace /* Replace existing file on disk with new contents. */
    | Skip /* File already on disk with identical contents. */
    | Write; /* File not present on disk. */

  let logFileAction = (fileAction, fileName) =>
    logItem(
      "%s  %s\n",
      switch (fileAction) {
      | NoMatch => "NoMatch"
      | Replace => "Replace"
      | Skip => "Skip   "
      | Write => "Write  "
      },
      fileName,
    );

  let readFromDisk = (~outputDir) => {
    let filesOnDisk =
      outputDir
      |> Sys.readdir
      |> Array.fold_left(
           (set, file) =>
             Filename.check_suffix(file, suffix) ?
               StringSet.add(Filename.concat(outputDir, file), set) : set,
           StringSet.empty,
         );
    logItem(
      "Found %d generated .re files in %s\n",
      filesOnDisk |> StringSet.cardinal,
      outputDir,
    );
    {filesOnDisk, filesToWrite: StringSet.empty};
  };

  let readLines = (file: string): list(string) => {
    let lines = ref([]);
    let chan = open_in(file);
    let finished_lines =
      try (
        {
          while (true) {
            lines := [input_line(chan), ...lines^];
          };
          [];
        }
      ) {
      | End_of_file =>
        close_in(chan);
        lines^ |> List.rev;
      };
    finished_lines;
  };

  let readFile = (file: string): string =>
    String.concat("\n", readLines(file));

  let writeFileIfRequired = (~fileName, ~fileContents, ~writeFile, x) => {
    x.filesToWrite = StringSet.add(fileName, x.filesToWrite);
    if (StringSet.mem(fileName, x.filesOnDisk)) {
      let oldContents = readFile(fileName);
      let identical = oldContents == fileContents;
      if (identical) {
        fileName |> logFileAction(Skip);
      };
      if (!identical) {
        fileName |> logFileAction(Replace);
        writeFile(fileName, fileContents);
      };
    } else {
      fileName |> logFileAction(Write);
      writeFile(fileName, fileContents);
    };
  };

  let cleanup = ({filesOnDisk, filesToWrite}) => {
    let filesToRemove = StringSet.diff(filesOnDisk, filesToWrite);
    if (!StringSet.is_empty(filesToRemove)) {
      logItem("Clean up %d .re files\n", filesToRemove |> StringSet.cardinal);
      StringSet.iter(
        file => {
          log("Delete %s\n", file);
          Unix.unlink(file);
        },
        filesToRemove,
      );
    } else {
      logItem("No .re files to clean up.\n");
    };
  };
};

let writeFile = (filePath: string, contents: string) => {
  let outFile = open_out(filePath);
  output_string(outFile, contents);
  close_out(outFile);
};

let emitCodeItems =
    (
      ~generatedFiles,
      ~inputPath,
      ~outputPath,
      ~fileHeader,
      ~signFile,
      codeItems,
    ) =>
  switch (codeItems) {
  | [_, ..._] =>
    let codeText = codeItems |> EmitJs.emitCodeItems;
    let fileContents = signFile(fileHeader ++ "\n" ++ codeText);

    generatedFiles
    |> GeneratedReFiles.writeFileIfRequired(
         ~fileName=outputPath,
         ~fileContents,
         ~writeFile,
       );

  | [] => outputPath |> GeneratedReFiles.logFileAction(NoMatch)
  };

let processCMTFile =
    (
      ~generatedFiles,
      ~modulesMap,
      ~outputDir,
      ~fileHeader,
      ~signFile,
      inputPath,
    ) => {
  GenIdent.resetPerFile();
  let inputCMT = Cmt_format.read_cmt(inputPath);
  let globalModuleName =
    Filename.chop_extension(Filename.basename(inputPath));
  let outputPath =
    Filename.concat(
      outputDir,
      outputReasonModuleName(globalModuleName) ++ suffix,
    );
  inputCMT
  |> cmtToCodeItems(~modulesMap, ~globalModuleName)
  |> emitCodeItems(
       ~generatedFiles,
       ~inputPath,
       ~outputPath,
       ~fileHeader,
       ~signFile,
     );
};

let run =
    (
      ~outputDir,
      ~fileHeader,
      ~signFile,
      ~modulesMap,
      ~findCmtFiles,
      ~buildSourceFiles,
      ~buildGeneratedFiles,
      ~doCleanup,
    ) => {
  buildSourceFiles();

  log("Looking for files with %s\n", tagSearch);
  let cmtFiles = findCmtFiles();
  cmtFiles |> List.iter(fileName => logItem("Found  %s\n", fileName));

  log("Searching for existing files on disk\n");
  let generatedFiles = GeneratedReFiles.readFromDisk(~outputDir);

  log("Generating .re files\n");
  cmtFiles
  |> List.iter(
       processCMTFile(
         ~generatedFiles,
         ~modulesMap,
         ~fileHeader,
         ~signFile,
         ~outputDir,
       ),
     );

  if (doCleanup) {
    log("Cleaning up\n");
    GeneratedReFiles.cleanup(generatedFiles);
  };

  buildGeneratedFiles();
  log("Done\n");
};