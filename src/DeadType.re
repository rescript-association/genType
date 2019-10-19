/* Adapted from https://github.com/LexiFi/dead_code_analyzer */

let collectExport =
    (path, u, {type_kind, type_manifest}: Types.type_declaration) => {
  let save = (id, loc) => {
    if (type_manifest == None) {
      DeadCommon.export(path, u, DeadCommon.typeDecs, id, loc);
    };
    let path =
      String.concat(".") @@ List.rev_map(id => id.Ident.name, [id, ...path]);
    Hashtbl.replace(DeadCommon.fields, path, loc.Location.loc_start);
  };

  switch (type_kind) {
  | Type_record(l, _) =>
    List.iter(
      ({Types.ld_id, ld_loc, ld_type, _}) => save(ld_id, ld_loc),
      l,
    )
  | Type_variant(l) =>
    List.iter(({Types.cd_id, cd_loc, _}) => save(cd_id, cd_loc), l)
  | _ => ()
  };
};

let collectReferences = (~posDeclaration, ~posUsage) =>
  DeadCommon.PosHash.addSet(
    DeadCommon.valueReferences,
    posDeclaration,
    posUsage,
  );