open SharedTypes;

let fileForCmt = (~moduleName, cmt, uri, processDoc) => {
  let%try infos = Shared.tryReadCmt(cmt);
  Ok(ProcessCmt.forCmt(~moduleName, uri, processDoc, infos));
};

let fullForCmt = (~moduleName, ~allLocations, cmt, uri, processDoc) => {
  let%try infos = Shared.tryReadCmt(cmt);
  let file = ProcessCmt.forCmt(~moduleName, uri, processDoc, infos);
  let extra = ProcessExtra.forCmt(~file, ~allLocations, infos);
  Ok({file, extra});
};

let astForCmt = cmt => {
  let%try infos = Shared.tryReadCmt(cmt);
  switch (infos.cmt_annots) {
  | Implementation(structure) =>
    /* The definition of my Compiler_libs_406 is different from ocaml-migrate-parsetree's in
       a couple small ways that don't actually change the in-memory representation :eyeroll: */
    Ok(`Implementation(Untypeast.untype_structure(structure)))
  /* Printast.implementation(Stdlib.Format.str_formatter, Untypeast.untype_structure(structure));
     Ok(Format.flush_str_formatter()); */
  | Interface(signature) =>
    Ok(`Interface(Untypeast.untype_signature(signature)))
  /* Printast.interface(Stdlib.Format.str_formatter, Untypeast.untype_signature(signature));
     Ok(Format.flush_str_formatter()); */

  | Partial_implementation(parts) =>
    let items =
      parts
      |> Array.to_list
      |> Utils.filterMap((p: Cmt_format.binary_part) =>
           switch (p) {
           | Partial_structure(str) => Some(str.str_items)
           | Partial_structure_item(str) => Some([str])
           /* | Partial_expression(exp) => Some([ str]) */
           | _ => None
           }
         )
      |> List.concat;
    Ok(
      `Implementation(
        List.map(
          item =>
            Untypeast.default_mapper.structure_item(
              Untypeast.default_mapper,
              item,
            ),
          items,
        ),
      ),
    );
  | Partial_interface(parts) =>
    let items =
      parts
      |> Array.to_list
      |> Utils.filterMap((p: Cmt_format.binary_part) =>
           switch (p) {
           | Partial_signature(str) => Some(str.sig_items)
           | Partial_signature_item(str) => Some([str])
           /* | Partial_expression(exp) => Some([ str]) */
           | _ => None
           }
         )
      |> List.concat;
    Ok(
      `Interface(
        List.map(
          item =>
            Untypeast.default_mapper.signature_item(
              Untypeast.default_mapper,
              item,
            ),
          items,
        ),
      ),
    );
  | _ => Error("Not a well-typed implementation")
  };
};

module PrintType = PrintType;
