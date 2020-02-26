/* Adapted from https://github.com/LexiFi/dead_code_analyzer */

open DeadCommon;

/* Keep track of the module path while traversing with Tast_mapper */
let modulePath: ref(list(string)) = ref([]);

let typeDependencies = ref([]);

let collectTypeExport =
    (
      ~implementationWithInterface,
      ~path,
      {type_kind, type_manifest}: Types.type_declaration,
    ) => {
  let save = (~decKind, ~id, ~loc) => {
    if (type_manifest == None) {
      export(~decKind, ~path, ~id, ~implementationWithInterface, ~loc);
    };
    let path =
      List.rev_map(id => id.Ident.name, [id, ...path]) |> String.concat(".");
    Hashtbl.replace(fields, path, loc.Location.loc_start);
  };

  switch (type_kind) {
  | Type_record(l, _) =>
    List.iter(
      ({Types.ld_id, ld_loc, ld_type}) =>
        save(~decKind=RecordLabel, ~id=ld_id, ~loc=ld_loc),
      l,
    )
  | Type_variant(l) =>
    List.iter(
      ({Types.cd_id, cd_loc}) =>
        save(~decKind=VariantCase, ~id=cd_id, ~loc=cd_loc),
      l,
    )
  | _ => ()
  };
};

let addTypeReference = (~posDeclaration, ~posUsage) => {
  if (verbose) {
    Log_.item(
      "[type] addTypeReference %s --> %s\n",
      posUsage |> posToString,
      posDeclaration |> posToString,
    );
  };
  PosHash.addSet(typeReferences, posDeclaration, posUsage);
};

let processTypeDeclaration = (typeDeclaration: Typedtree.type_declaration) => {
  let updateDependencies = (name, pos) => {
    let path =
      [
        currentModuleName^,
        ...List.rev([
             name.Asttypes.txt,
             typeDeclaration.typ_name.txt,
             ...modulePath^,
           ]),
      ]
      |> String.concat(".");

    try(
      switch (typeDeclaration.typ_manifest) {
      | Some({ctyp_desc: Ttyp_constr(_, {txt}, _)}) =>
        let pos1 =
          Hashtbl.find(
            fields,
            [currentModuleName^, ...Longident.flatten(txt)]
            @ [name.Asttypes.txt]
            |> String.concat("."),
          );

        let pos2 = Hashtbl.find(fields, path);
        typeDependencies :=
          [(pos2, pos1), (pos1, pos), ...typeDependencies^];
      | _ => ()
      }
    ) {
    | _ => ()
    };
    try({
      let pos1 = Hashtbl.find(fields, path);
      typeDependencies := [(pos1, pos), ...typeDependencies^];
    }) {
    | Not_found => Hashtbl.add(fields, path, pos)
    };
  };

  switch (typeDeclaration.typ_kind) {
  | Ttype_record(l) =>
    l
    |> List.iter(({Typedtree.ld_name, ld_loc, ld_type}) =>
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