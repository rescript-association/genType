/***************************************************************************/
/*                                                                         */
/*   Copyright (c) 2014-2016 LexiFi SAS. All rights reserved.              */
/*                                                                         */
/*   This source code is licensed under the MIT License                    */
/*   found in the LICENSE file at the root of this source tree             */
/*                                                                         */
/***************************************************************************/

/********   ATTRIBUTES  ********/

let dependencies = ref([]); /* like the cmt value_dependencies but for types */

/********   HELPERS   ********/

let is_unit = t =>
  switch (Ctype.repr(t).desc) {
  | [@implicit_arity] Tconstr(p, [], _) => Path.same(p, Predef.path_unit)
  | _ => false
  };

let nb_args = (~keep, {desc}: Types.type_expr) => {
  let rec loop = (n, typeDesc: Types.type_desc) =>
    switch (typeDesc) {
    | [@implicit_arity] Tarrow(_, _, typ, _) when keep == `All =>
      loop(n + 1, typ.desc)
    | [@implicit_arity] Tarrow(Labelled(_), _, typ, _) when keep == `Lbl =>
      loop(n + 1, typ.desc)
    | [@implicit_arity] Tarrow(Optional(_), _, typ, _) when keep == `Opt =>
      loop(n + 1, typ.desc)
    | [@implicit_arity] Tarrow(Nolabel, _, typ, _) when keep == `Reg =>
      loop(n + 1, typ.desc)
    | [@implicit_arity] Tarrow(_, _, typ, _) => loop(n, typ.desc)
    | _ => n
    };

  loop(0, desc);
};

let is_type = s => {
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

/********   PROCESSING  ********/

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

let collect_references = (~posDeclaration, ~posUsage) =>
  DeadCommon.PosHash.addSet(
    DeadCommon.valueReferences,
    posDeclaration,
    posUsage,
  );

let tstr = ({typ_kind, typ_manifest, typ_name}: Typedtree.type_declaration) => {
  let assoc = (name, pos) => {
    let path =
      String.concat(".") @@
      List.rev @@
      [name.Asttypes.txt, typ_name.Asttypes.txt]
      @ [
        String.capitalize_ascii(
          DeadCommon.getModuleName(DeadCommon.currentSrc^),
        ),
      ];

    try(
      switch (typ_manifest) {
      | Some({ctyp_desc: [@implicit_arity] Ttyp_constr(_, {txt, _}, _), _}) =>
        let pos1 =
          Hashtbl.find(
            DeadCommon.fields,
            String.concat(".") @@
            [
              String.capitalize_ascii(
                DeadCommon.getModuleName(DeadCommon.currentSrc^),
              ),
              ...Longident.flatten(txt),
            ]
            @ [name.Asttypes.txt],
          );

        let pos2 = Hashtbl.find(DeadCommon.fields, path);
        dependencies := [(pos2, pos1), (pos1, pos), ...dependencies^];
      | _ => ()
      }
    ) {
    | _ => ()
    };
    try({
      let pos1 = Hashtbl.find(DeadCommon.fields, path);
      dependencies := [(pos1, pos), ...dependencies^];
    }) {
    | Not_found => Hashtbl.add(DeadCommon.fields, path, pos)
    };
  };

  switch (typ_kind) {
  | Ttype_record(l) =>
    List.iter(
      ({Typedtree.ld_name, ld_loc, ld_type, _}) =>
        assoc(ld_name, ld_loc.Location.loc_start),
      l,
    )
  | Ttype_variant(l) =>
    List.iter(
      ({Typedtree.cd_name, cd_loc, _}) =>
        assoc(cd_name, cd_loc.Location.loc_start),
      l,
    )
  | _ => ()
  };
};