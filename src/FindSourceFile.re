let rec interface = items =>
  switch (items) {
  | [{Typedtree.sig_loc}, ...rest] =>
    !Sys.file_exists(sig_loc.loc_start.pos_fname)
      ? interface(rest) : Some(sig_loc.loc_start.pos_fname)
  | [] => None
  };
let rec implementation = items =>
  switch (items) {
  | [{Typedtree.str_loc}, ...rest] =>
    !Sys.file_exists(str_loc.loc_start.pos_fname)
      ? implementation(rest) : Some(str_loc.loc_start.pos_fname)
  | [] => None
  };
let cmt = cmt_annots =>
  switch (cmt_annots) {
  | Cmt_format.Interface(signature) => interface(signature.sig_items)
  | Implementation(structure) => implementation(structure.str_items)
  | _ => None
  };
