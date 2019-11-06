/* Adapted from https://github.com/LexiFi/dead_code_analyzer */

open DeadCommon;

/* Keep track of the module path while traversing with Tast_mapper */
let modulePath: ref(list(string)) = ref([]);

let typeDependencies = ref([]);

let collectTypeExport =
    (~path, {type_kind, type_manifest}: Types.type_declaration) => {
  let save = (id, loc) => {
    if (type_manifest == None) {
      export(~analysisKind=Type, ~path, ~id, ~loc);
    };
    let path =
      List.rev_map(id => id.Ident.name, [id, ...path]) |> String.concat(".");
    Hashtbl.replace(fields, path, loc.Location.loc_start);
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

let addTypeReference = (~posDeclaration, ~posUsage) => {
  if (verbose) {
    GenTypeCommon.logItem(
      "[type] addTypeReference %s --> %s\n",
      posUsage |> posToString(~printCol=true, ~shortFile=true),
      posDeclaration |> posToString(~printCol=true, ~shortFile=true),
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
      | Some({ctyp_desc: Ttyp_constr(_, {txt, _}, _), _}) =>
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