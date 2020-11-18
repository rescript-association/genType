open Typedtree;
open SharedTypes;
open Infix;

let itemsExtent = items => {
  Typedtree.(
    items == []
      ? Location.none
      : {
        let first = List.hd(items);
        let last = List.nth(items, List.length(items) - 1);
        let (first, last) =
          first.str_loc.loc_start.pos_cnum < last.str_loc.loc_start.pos_cnum
            ? (first, last) : (last, first);

        {
          loc_ghost: true,
          loc_start: first.str_loc.loc_start,
          loc_end: last.str_loc.loc_end,
        };
      }
  );
};

let sigItemsExtent = items => {
  Typedtree.(
    items == []
      ? Location.none
      : {
        let first = List.hd(items);
        let last = List.nth(items, List.length(items) - 1);

        {
          Location.loc_ghost: true,
          loc_start: first.sig_loc.loc_start,
          loc_end: last.sig_loc.loc_end,
        };
      }
  );
};

type env = {
  stamps,
  processDoc: string => string,
  modulePath: visibilityPath,
  scope: Location.t,
};

let newDeclared = ProcessAttributes.newDeclared;

let addItem =
    (~name, ~extent, ~stamp, ~env, ~item, attributes, exported, stamps) => {
  let declared =
    newDeclared(
      ~item,
      ~scope={
        Location.loc_start: extent.Location.loc_end,
        loc_end: env.scope.loc_end,
        loc_ghost: false,
      },
      ~extent,
      ~name,
      ~stamp,
      ~modulePath=env.modulePath,
      ~processDoc=env.processDoc,
      !Hashtbl.mem(exported, name.txt),
      attributes,
    );
  if (!Hashtbl.mem(exported, name.txt)) {
    Hashtbl.add(exported, name.txt, stamp);
  };
  Hashtbl.add(stamps, stamp, declared);
  declared;
};

let rec forSignatureTypeItem = (env, exported: SharedTypes.exported, item) => {
  Types.(
    switch (item) {
    | Sig_value(ident, {val_type, val_attributes, val_loc: loc}) =>
      let item = val_type;
      let declared =
        addItem(
          ~name=Location.mknoloc(Ident.name(ident)),
          ~extent=loc,
          ~stamp=Ident.binding_time(ident),
          ~env,
          ~item,
          val_attributes,
          exported.values,
          env.stamps.values,
        );
      [{...declared, item: MValue(declared.item)}];
    | Sig_type(
        ident,
        {type_loc, type_kind, type_manifest, type_attributes} as decl,
        _,
      ) =>
      let declared =
        addItem(
          ~extent=type_loc,
          ~item={
            Type.decl,
            kind:
              switch (type_kind) {
              | Type_abstract =>
                switch (type_manifest) {
                | Some({desc: Tconstr(path, args, _)}) =>
                  Abstract(Some((path, args)))
                | Some({desc: Ttuple(items)}) => Tuple(items)
                /* TODO dig */
                | _ => Abstract(None)
                }
              | Type_open => Open
              | Type_variant(constructors) =>
                Variant(
                  constructors
                  |> List.map(
                       ({cd_loc, cd_id, cd_args, cd_res, cd_attributes}) => {
                       let name = Ident.name(cd_id);
                       let stamp = Ident.binding_time(cd_id);
                       let item = {
                         stamp,
                         cname: Location.mknoloc(name),
                         args:
                           (
                             switch (cd_args) {
                             | Cstr_tuple(args) => args
                             /* TODO(406): constructor record args support */
                             | Cstr_record(_) => []
                             }
                           )
                           |> List.map(t => (t, Location.none)),
                         res: cd_res,
                       };
                       let declared =
                         newDeclared(
                           ~item,
                           ~extent=cd_loc,
                           ~scope={
                             Location.loc_start: type_loc.Location.loc_end,
                             loc_end: env.scope.loc_end,
                             loc_ghost: false,
                           },
                           ~name=Location.mknoloc(name),
                           ~stamp,
                           /* TODO maybe this needs another child */
                           ~modulePath=env.modulePath,
                           ~processDoc=env.processDoc,
                           true,
                           cd_attributes,
                         );
                       Hashtbl.add(env.stamps.constructors, stamp, declared);
                       item;
                     }),
                )
              | Type_record(labels, _) =>
                Record(
                  labels
                  |> List.map(({ld_id, ld_type}) => {
                       let astamp = Ident.binding_time(ld_id);
                       let name = Ident.name(ld_id);
                       {
                         stamp: astamp,
                         aname: Location.mknoloc(name),
                         typ: ld_type,
                       };
                     }),
                )
              },
          },
          ~name=Location.mknoloc(Ident.name(ident)),
          ~stamp=Ident.binding_time(ident),
          ~env,
          type_attributes,
          exported.types,
          env.stamps.types,
        );
      [{...declared, item: MType(declared.item)}];
    /* | Sig_module({stamp, name}, {md_type: Mty_ident(path) | Mty_alias(path), md_attributes, md_loc}, _) =>
       let declared = addItem(~contents=Module.Ident(path), ~name=Location.mknoloc(name), ~stamp, ~env, md_attributes, exported.modules, env.stamps.modules);
       [{...declared, contents: Module.Module(declared.contents)}, ...items] */
    | Sig_module(ident, {md_type, md_attributes, md_loc}, _) =>
      let declared =
        addItem(
          ~extent=md_loc,
          ~item=forModuleType(env, md_type),
          ~name=Location.mknoloc(Ident.name(ident)),
          ~stamp=Ident.binding_time(ident),
          ~env,
          md_attributes,
          exported.modules,
          env.stamps.modules,
        );
      [{...declared, item: Module(declared.item)}];
    | _ => []
    }
  );
}

and forSignatureType = (env, signature) => {
  let exported = initExported();
  let topLevel =
    List.fold_right(
      (item, items) => {forSignatureTypeItem(env, exported, item) @ items},
      signature,
      [],
    )
    |> List.rev;

  {exported, topLevel};
}
and forModuleType = (env, moduleType) =>
  switch (moduleType) {
  | Types.Mty_ident(path) => Ident(path)
  | Mty_alias(_ /* 402*/, path) => Ident(path)
  | Mty_signature(signature) => Structure(forSignatureType(env, signature))
  | Mty_functor(_argIdent, _argType, resultType) =>
    forModuleType(env, resultType)
  };

let getModuleTypePath = mod_desc =>
  switch (mod_desc) {
  | Tmty_ident(path, _)
  | Tmty_alias(path, _) => Some(path)
  | Tmty_signature(_)
  | Tmty_functor(_)
  | Tmty_with(_)
  | Tmty_typeof(_) => None
  };

let forTypeDeclaration =
    (
      ~env,
      ~exported: exported,
      {
        typ_id,
        typ_loc,
        typ_name: name,
        typ_attributes,
        typ_type,
        typ_kind,
        typ_manifest,
      },
    ) => {
  let stamp = Ident.binding_time(typ_id);
  let declared =
    addItem(
      ~extent=typ_loc,
      ~item={
        Type.decl: typ_type,
        kind:
          switch (typ_kind) {
          | Ttype_abstract =>
            switch (typ_manifest) {
            | Some({ctyp_desc: Ttyp_constr(path, _lident, args)}) =>
              Abstract(Some((path, args |> List.map(t => t.ctyp_type))))
            | Some({ctyp_desc: Ttyp_tuple(items)}) =>
              Tuple(items |> List.map(t => t.ctyp_type))
            /* TODO dig */
            | _ => Abstract(None)
            }
          | Ttype_open => Open
          | Ttype_variant(constructors) =>
            Variant(
              constructors
              |> List.map(({cd_id, cd_name: cname, cd_args, cd_res}) => {
                   let stamp = Ident.binding_time(cd_id);
                   {
                     stamp,
                     cname,
                     args:
                       (
                         switch (cd_args) {
                         | Cstr_tuple(args) => args
                         /* TODO(406) */
                         | Cstr_record(_) => []
                         }
                       )
                       |> List.map(t => (t.ctyp_type, t.ctyp_loc)),
                     res: cd_res |?>> (t => t.ctyp_type),
                   };
                 }),
            )
          | Ttype_record(labels) =>
            Record(
              labels
              |> List.map(({ld_id, ld_name: aname, ld_type: {ctyp_type}}) => {
                   let astamp = Ident.binding_time(ld_id);
                   {stamp: astamp, aname, typ: ctyp_type};
                 }),
            )
          },
      },
      ~name,
      ~stamp,
      ~env,
      typ_attributes,
      exported.types,
      env.stamps.types,
    );
  {...declared, item: MType(declared.item)};
};

let forSignatureItem = (~env, ~exported: exported, item) => {
  switch (item.sig_desc) {
  | Tsig_value({val_id, val_loc, val_name: name, val_desc, val_attributes}) =>
    let declared =
      addItem(
        ~name,
        ~stamp=Ident.binding_time(val_id),
        ~extent=val_loc,
        ~item=val_desc.ctyp_type,
        ~env,
        val_attributes,
        exported.values,
        env.stamps.values,
      );
    [{...declared, item: MValue(declared.item)}];
  | Tsig_type(_ /*402*/, decls) =>
    decls |> List.map(forTypeDeclaration(~env, ~exported))
  | Tsig_module({
      md_id,
      md_attributes,
      md_loc,
      md_name: name,
      md_type: {mty_type},
    }) =>
    let item = forModuleType(env, mty_type);
    let declared =
      addItem(
        ~item,
        ~name,
        ~extent=md_loc,
        ~stamp=Ident.binding_time(md_id),
        ~env,
        md_attributes,
        exported.modules,
        env.stamps.modules,
      );
    [{...declared, item: Module(declared.item)}];
  | Tsig_include({incl_mod, incl_type}) =>
    let env =
      switch (getModuleTypePath(incl_mod.mty_desc)) {
      | None => env
      | Some(path) => {
          ...env,
          modulePath: IncludedModule(path, env.modulePath),
        }
      };
    let topLevel =
      List.fold_right(
        (item, items) => {forSignatureTypeItem(env, exported, item) @ items},
        incl_type,
        [],
      )
      |> List.rev;

    topLevel;
  /* TODO: process other things here */
  | _ => []
  };
};

let forSignature = (~env, items) => {
  let exported = initExported();
  let topLevel =
    items |> List.map(forSignatureItem(~env, ~exported)) |> List.flatten;
  {exported, topLevel};
};

let forTreeModuleType = (~env, {mty_desc}) =>
  switch (mty_desc) {
  | Tmty_ident(_) => None
  | Tmty_signature({sig_items}) =>
    let contents = forSignature(~env, sig_items);
    Some(Structure(contents));
  | _ => None
  };

let rec getModulePath = mod_desc =>
  switch (mod_desc) {
  | Tmod_ident(path, _lident) => Some(path)
  | Tmod_structure(_) => None
  | Tmod_functor(_ident, _argName, _maybeType, _resultExpr) => None
  | Tmod_apply(functor_, _arg, _coercion) => getModulePath(functor_.mod_desc)
  | Tmod_unpack(_expr, _moduleType) => None
  | Tmod_constraint(expr, _typ, _constraint, _coercion) =>
    getModulePath(expr.mod_desc)
  };

let rec forItem = (~env, ~exported: exported, item) =>
  switch (item.str_desc) {
  | Tstr_value(_isRec, bindings) =>
    optMap(
      ({vb_loc, vb_pat: {pat_desc, pat_type}, vb_attributes}) =>
        /* TODO get all the things out of the var. */
        switch (pat_desc) {
        | Tpat_var(ident, name) =>
          let item = pat_type;
          let declared =
            addItem(
              ~name,
              ~stamp=Ident.binding_time(ident),
              ~env,
              ~extent=vb_loc,
              ~item,
              vb_attributes,
              exported.values,
              env.stamps.values,
            );
          Some({...declared, item: MValue(declared.item)});
        | _ => None
        },
      bindings,
    )
  | Tstr_module({
      mb_id,
      mb_attributes,
      mb_loc,
      mb_name: name,
      mb_expr: {mod_desc},
    }) =>
    let item = forModule(env, mod_desc, name.txt);
    let declared =
      addItem(
        ~item,
        ~name,
        ~extent=mb_loc,
        ~stamp=Ident.binding_time(mb_id),
        ~env,
        mb_attributes,
        exported.modules,
        env.stamps.modules,
      );
    [{...declared, item: Module(declared.item)}];
  | Tstr_include({incl_mod, incl_type}) =>
    let env =
      switch (getModulePath(incl_mod.mod_desc)) {
      | None => env
      | Some(path) => {
          ...env,
          modulePath: IncludedModule(path, env.modulePath),
        }
      };
    let topLevel =
      List.fold_right(
        (item, items) => {forSignatureTypeItem(env, exported, item) @ items},
        incl_type,
        [],
      )
      |> List.rev;

    topLevel;

  | Tstr_primitive({
      val_id,
      val_name: name,
      val_loc,
      val_attributes,
      val_val: {val_type},
    }) =>
    let declared =
      addItem(
        ~extent=val_loc,
        ~item=val_type,
        ~name,
        ~stamp=Ident.binding_time(val_id),
        ~env,
        val_attributes,
        exported.values,
        env.stamps.values,
      );
    [{...declared, item: MValue(declared.item)}];
  | Tstr_type(_, decls) =>
    decls |> List.map(forTypeDeclaration(~env, ~exported))
  | _ => []
  }

and forModule = (env, mod_desc, moduleName) =>
  switch (mod_desc) {
  | Tmod_ident(path, _lident) => Ident(path)
  | Tmod_structure(structure) =>
    let env = {
      ...env,
      scope: itemsExtent(structure.str_items),
      modulePath: ExportedModule(moduleName, env.modulePath),
    };
    let contents = forStructure(~env, structure.str_items);
    Structure(contents);
  | Tmod_functor(ident, argName, maybeType, resultExpr) =>
    maybeType
    |?< (
      t =>
        forTreeModuleType(~env, t)
        |?< (
          kind => {
            let stamp = Ident.binding_time(ident);
            let declared =
              newDeclared(
                ~item=kind,
                ~name=argName,
                ~scope={
                  Location.loc_start: t.mty_loc.loc_end,
                  loc_end: env.scope.loc_end,
                  loc_ghost: false,
                },
                ~extent=t.Typedtree.mty_loc,
                ~stamp,
                ~modulePath=NotVisible,
                ~processDoc=env.processDoc,
                false,
                [],
              );
            Hashtbl.add(env.stamps.modules, stamp, declared);
          }
        )
    );
    forModule(env, resultExpr.mod_desc, moduleName);
  | Tmod_apply(functor_, _arg, _coercion) =>
    forModule(env, functor_.mod_desc, moduleName)
  | Tmod_unpack(_expr, moduleType) =>
    let env = {
      ...env,
      modulePath: ExportedModule(moduleName, env.modulePath),
    };
    forModuleType(env, moduleType);
  | Tmod_constraint(_expr, typ, _constraint, _coercion) =>
    /* TODO do this better I think */
    let env = {
      ...env,
      modulePath: ExportedModule(moduleName, env.modulePath),
    };
    forModuleType(env, typ);
  }

and forStructure = (~env, items) => {
  let exported = initExported();
  let topLevel =
    List.fold_right(
      (item, results) => {forItem(~env, ~exported, item) @ results},
      items,
      [],
    );
  {exported, topLevel};
};

let forCmt =
    (
      ~moduleName,
      uri,
      processDoc,
      {cmt_modname, cmt_annots}: Cmt_format.cmt_infos,
    ) =>
  switch (cmt_annots) {
  | Partial_implementation(parts) =>
    let items =
      parts
      |> Array.to_list
      |> Utils.filterMap(p =>
           switch ((p: Cmt_format.binary_part)) {
           | Partial_structure(str) => Some(str.str_items)
           | Partial_structure_item(str) => Some([str])
           | _ => None
           }
         )
      |> List.concat;
    let extent = itemsExtent(items);
    let extent = {
      ...extent,
      loc_end: {
        ...extent.loc_end,
        pos_lnum: extent.loc_end.pos_lnum + 1000000,
        pos_cnum: extent.loc_end.pos_cnum + 100000000,
      },
    };
    let env = {
      scope: extent,
      stamps: initStamps(),
      processDoc,
      modulePath: File(uri, moduleName),
    };
    let contents = forStructure(~env, items);
    {uri, moduleName: cmt_modname, stamps: env.stamps, contents};
  | Partial_interface(parts) =>
    let items =
      parts
      |> Array.to_list
      |> Utils.filterMap((p: Cmt_format.binary_part) =>
           switch (p) {
           | Partial_signature(str) => Some(str.sig_items)
           | Partial_signature_item(str) => Some([str])
           | _ => None
           }
         )
      |> List.concat;
    let env = {
      scope: sigItemsExtent(items),
      stamps: initStamps(),
      processDoc,
      modulePath: File(uri, moduleName),
    };
    let contents = forSignature(~env, items);
    {uri, moduleName: cmt_modname, stamps: env.stamps, contents};
  | Implementation(structure) =>
    let env = {
      scope: itemsExtent(structure.str_items),
      stamps: initStamps(),
      processDoc,
      modulePath: File(uri, moduleName),
    };
    let contents = forStructure(~env, structure.str_items);
    {uri, moduleName: cmt_modname, stamps: env.stamps, contents};
  | Interface(signature) =>
    let env = {
      scope: sigItemsExtent(signature.sig_items),
      stamps: initStamps(),
      processDoc,
      modulePath: File(uri, moduleName),
    };
    let contents = forSignature(~env, signature.sig_items);
    {uri, moduleName: cmt_modname, stamps: env.stamps, contents};
  | _ => SharedTypes.emptyFile(moduleName, uri)
  };
