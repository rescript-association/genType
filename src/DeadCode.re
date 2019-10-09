/***************************************************************************/
/*                                                                         */
/*   Copyright (c) 2014-2016 LexiFi SAS. All rights reserved.              */
/*                                                                         */
/*   This source code is licensed under the MIT License                    */
/*   found in the LICENSE file at the root of this source tree             */
/*                                                                         */
/***************************************************************************/

/** Dead code anlyzing tool. It only reports unused exported values, constructors/record fields
  and methods by default.
  Options can enable reporting of optional arguments always/never used as bad style of code.
  In addition to selecting which reports are to be displayed, the limit of authorized
  occurences needed to be reported can be selected (default is 0).
  It assumes .mli/.mfi are compiled with -keep-locs and .ml/.mf are compiled with -bin-annot.
 */;

/********   PROCESSING   ********/

open DeadCommon;

/* Starting point */
let load_file = (~sourceFile, cmtFilePath) => {
  lastPos := Lexing.dummy_pos;
  if (verbose) {
    GenTypeCommon.logItem("Scanning %s\n", cmtFilePath);
  };
  currentSrc := sourceFile;
  let {Cmt_format.cmt_annots, cmt_value_dependencies} =
    Cmt_format.read_cmt(cmtFilePath);
  switch (cmt_annots) {
  | Interface(signature) =>
    ProcessAnnotations.signature(signature);
    DeadValue.process_signature(cmtFilePath, signature.sig_type);
  | Implementation(structure) =>
    let cmtiExists =
      Sys.file_exists((cmtFilePath |> Filename.chop_extension) ++ ".cmti");
    if (!cmtiExists) {
      ProcessAnnotations.structure(structure);
    };
    DeadValue.processStructure(
      ~cmtiExists,
      cmt_value_dependencies,
      structure,
    );
    if (!cmtiExists) {
      DeadValue.process_signature(cmtFilePath, structure.str_type);
    };
  | _ => ()
  };
};

let report = (~onDeadValue) => {
  let onItem = ({pos, path}) => {
    print_string(pos |> posToString);
    print_string(path);
    print_newline();
  };
  let onDeadValue = item => {
    onItem(item);
    onDeadValue(item);
  };
  Printf.printf("\n%s:\n", "UNUSED EXPORTED VALUES");
  valueDecs |> report(~onItem=onDeadValue);
  Printf.printf("\n%s:\n", "UNUSED CONSTRUCTORS/RECORD FIELDS");
  typeDecs |> report(~onItem);
};