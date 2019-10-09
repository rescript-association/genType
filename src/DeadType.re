/***************************************************************************/
/*                                                                         */
/*   Copyright (c) 2014-2016 LexiFi SAS. All rights reserved.              */
/*                                                                         */
/*   This source code is licensed under the MIT License                    */
/*   found in the LICENSE file at the root of this source tree             */
/*                                                                         */
/***************************************************************************/

/********   ATTRIBUTES  ********/

let isType = s => {
  let rec blk = (s, p, l, acc) =>
    try(
      if (s.[p] == '.') {
        let acc = [String.sub(s, p - l, l), ...acc];
        blk(s, p + 1, 0, acc);
      } else {
        blk(s, p + 1, l + 1, acc);
      }
    ) {
    | _ => [String.sub(s, p - l, l), ...acc]
    };

  if (!String.contains(s, '.')) {
    false;
  } else {
    switch (blk(s, 0, 0, [])) {
    | [hd, cont, ..._] =>
      String.capitalize_ascii(hd) == hd
      || String.lowercase_ascii(cont) == cont
    | _ => assert(false)
    };
  };
};

let collect_export =
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
  | [@implicit_arity] Type_record(l, _) =>
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