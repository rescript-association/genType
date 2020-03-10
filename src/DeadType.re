/* Adapted from https://github.com/LexiFi/dead_code_analyzer */

open DeadCommon;

let typeDependencies = ref([]);

let addTypeDeclaration =
    (~path, {type_kind, type_manifest}: Types.type_declaration) => {
  let save = (~declKind, ~loc, ~name) => {
    if (type_manifest == None) {
      addDeclaration(~declKind, ~path, ~loc, ~name);
    };
    let path = [name, ...path] |> pathToString;
    Hashtbl.replace(fields, path, loc.Location.loc_start);
  };

  switch (type_kind) {
  | Type_record(l, _) =>
    List.iter(
      ({Types.ld_id, ld_loc}) =>
        save(~declKind=RecordLabel, ~loc=ld_loc, ~name=Ident.name(ld_id)),
      l,
    )
  | Type_variant(l) =>
    List.iter(
      ({Types.cd_id, cd_loc}) =>
        save(~declKind=VariantCase, ~loc=cd_loc, ~name=Ident.name(cd_id)),
      l,
    )
  | _ => ()
  };
};

let addTypeReference = (~posDeclaration, ~posUsage) => {
  if (verbose) {
    Log_.item(
      "[type] addTypeReference %s --> %s@.",
      posUsage |> posToString,
      posDeclaration |> posToString,
    );
  };
  PosHash.addSet(typeReferences, posDeclaration, posUsage);
};

let processTypeDeclaration = (typeDeclaration: Typedtree.type_declaration) => {
  let extendTypeDependencies = (pos1, pos2) =>
    if (pos1 != pos2) {
      if (verbose) {
        Log_.item(
          "[type] extendTypeDependencies %s --> %s@.",
          pos1 |> posToString,
          pos2 |> posToString,
        );
      };

      typeDependencies := [(pos1, pos2), ...typeDependencies^];
    };
  let updateDependencies = (name, pos) => {
    let path2 =
      [
        currentModuleName^,
        ...List.rev([
             name.Asttypes.txt,
             typeDeclaration.typ_name.txt,
             ...currentModulePath^,
           ]),
      ]
      |> String.concat(".");

    try(
      switch (typeDeclaration.typ_manifest) {
      | Some({ctyp_desc: Ttyp_constr(_, {txt}, _)}) =>
        let path1 =
          [currentModuleName^, ...Longident.flatten(txt)]
          @ [name.Asttypes.txt]
          |> String.concat(".");
        let pos1 = Hashtbl.find(fields, path1);
        let pos2 = Hashtbl.find(fields, path2);
        extendTypeDependencies(pos, pos1);
        extendTypeDependencies(pos1, pos2);
      | _ => ()
      }
    ) {
    | _ => ()
    };
    switch (Hashtbl.find_opt(fields, path2)) {
    | Some(pos2) => extendTypeDependencies(pos, pos2)
    | None => Hashtbl.add(fields, path2, pos)
    };
  };

  switch (typeDeclaration.typ_kind) {
  | Ttype_record(l) =>
    l
    |> List.iter(({Typedtree.ld_name, ld_loc}) =>
         updateDependencies(ld_name, ld_loc.Location.loc_start)
       )

  | Ttype_variant(l) =>
    l
    |> List.iter(({Typedtree.cd_name, cd_loc}) =>
         updateDependencies(cd_name, cd_loc.Location.loc_start)
       )

  | _ => ()
  };
};