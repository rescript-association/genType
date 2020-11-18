open Typedtree;
open SharedTypes;
open Infix;

let handleConstructor = (path, txt) => {
  let typeName =
    switch (path) {
    | Path.Pdot(_path, typename, _) => typename
    | Pident(ident) => Ident.name(ident)
    | _ => assert(false)
    };
  Longident.(
    switch (txt) {
    | Longident.Lident(name) => (name, Lident(typeName))
    | Ldot(left, name) => (name, Ldot(left, typeName))
    | Lapply(_, _) => assert(false)
    }
  );
};

let rec relative = (ident, path) =>
  switch (ident, path) {
  | (Longident.Lident(name), Path.Pdot(path, pname, _)) when pname == name =>
    Some(path)
  | (Longident.Ldot(ident, name), Path.Pdot(path, pname, _))
      when pname == name =>
    relative(ident, path)
  /* | (Ldot(Lident("*predef*" | "exn"), _), Pident(_)) => None */
  | _ => None
  };

let findClosestMatchingOpen = (opens, path, ident, loc) => {
  let%opt openNeedle = relative(ident, path);

  let matching =
    Hashtbl.fold(
      (_, op, res) =>
        if (Utils.locWithinLoc(loc, op.extent)
            && Path.same(op.path, openNeedle)) {
          [op, ...res];
        } else {
          res;
        },
      opens,
      [],
    )
    |> List.sort((a: SharedTypes.openTracker, b) => {
         b.loc.loc_start.pos_cnum - a.loc.loc_start.pos_cnum
       });

  switch (matching) {
  | [] => None
  | [first, ..._] => Some(first)
  };
};

let getTypeAtPath = (~env, path) => {
  switch (Query.fromCompilerPath(~env, path)) {
  | `GlobalMod(_) => `Not_found
  | `Global(moduleName, path) => `Global((moduleName, path))
  | `Not_found => `Not_found
  | `Exported(env, name) =>
    let res = {
      let%opt stamp = Hashtbl.find_opt(env.exported.types, name);
      let%opt_wrap declaredType =
        Hashtbl.find_opt(env.file.stamps.types, stamp);
      `Local(declaredType);
    };
    res |? `Not_found;
  | `Stamp(stamp) =>
    let res = {
      let%opt_wrap declaredType =
        Hashtbl.find_opt(env.file.stamps.types, stamp);
      `Local(declaredType);
    };
    res |? `Not_found;
  };
};

module F =
       (
         Collector: {
           let extra: extra;
           let file: file;
           let scopeExtent: ref(list(Location.t));
           let allLocations: bool;
         },
       ) => {
  let extra = Collector.extra;

  let maybeAddUse = (path, ident, loc, tip) => {
    let%opt_consume tracker =
      findClosestMatchingOpen(extra.opens, path, ident, loc);
    let%opt_consume relpath = Query.makeRelativePath(tracker.path, path);

    tracker.used = [(relpath, tip, loc), ...tracker.used];
  };

  let addLocation = (loc, ident) =>
    extra.locations = [(loc, ident), ...extra.locations];
  let addReference = (stamp, loc) =>
    Hashtbl.replace(
      extra.internalReferences,
      stamp,
      [
        loc,
        ...Hashtbl.mem(extra.internalReferences, stamp)
             ? Hashtbl.find(extra.internalReferences, stamp) : [],
      ],
    );
  let addExternalReference = (moduleName, path, tip, loc) => {
    /* TODO need to follow the path, and be able to load the files to follow module references... */
    Hashtbl.replace(
      extra.externalReferences,
      moduleName,
      [
        (path, tip, loc),
        ...Hashtbl.mem(extra.externalReferences, moduleName)
             ? Hashtbl.find(extra.externalReferences, moduleName) : [],
      ],
    );
  };
  let env = {
    Query.file: Collector.file,
    exported: Collector.file.contents.exported,
  };

  let getTypeAtPath = getTypeAtPath(~env);

  let addForPath = (path, lident, loc, typ, tip) => {
    maybeAddUse(path, lident, loc, tip);
    let identName = Longident.last(lident);
    let identLoc = Utils.endOfLocation(loc, String.length(identName));
    let locType =
      switch (Query.fromCompilerPath(~env, path)) {
      | `Stamp(stamp) =>
        addReference(stamp, identLoc);
        LocalReference(stamp, tip);
      | `Not_found => NotFound
      | `Global(moduleName, path) =>
        addExternalReference(moduleName, path, tip, identLoc);
        GlobalReference(moduleName, path, tip);
      | `Exported(env, name) =>
        let res = {
          let%opt_wrap stamp = Hashtbl.find_opt(env.exported.values, name);
          addReference(stamp, identLoc);
          LocalReference(stamp, tip);
        };
        res |? NotFound;
      | `GlobalMod(_) => NotFound
      };
    addLocation(loc, Typed(typ, locType));
  };

  let addForPathParent = (path, loc) => {
    let locType =
      switch (Query.fromCompilerPath(~env, path)) {
      | `GlobalMod(name) =>
        /* TODO track external references to filenames to handle renames well */
        TopLevelModule(name)
      | `Stamp(stamp) =>
        addReference(stamp, loc);
        LModule(LocalReference(stamp, Module));
      | `Not_found => LModule(NotFound)
      | `Global(moduleName, path) =>
        addExternalReference(moduleName, path, Module, loc);
        LModule(GlobalReference(moduleName, path, Module));
      | `Exported(env, name) =>
        let res = {
          let%opt_wrap stamp = Hashtbl.find_opt(env.exported.modules, name);
          addReference(stamp, loc);
          LModule(LocalReference(stamp, Module));
        };
        res |? LModule(NotFound);
      };
    addLocation(loc, locType);
  };

  let addForField = (recordType, item, {Asttypes.txt, loc}) => {
    switch (Shared.dig(recordType).desc) {
    | Tconstr(path, _args, _memo) =>
      let t = getTypeAtPath(path);
      let {Types.lbl_res} = item;

      let (name, typeLident) = handleConstructor(path, txt);
      maybeAddUse(path, typeLident, loc, Attribute(name));

      let nameLoc = Utils.endOfLocation(loc, String.length(name));
      let locType =
        switch (t) {
        | `Local({stamp, item: {kind: Record(attributes)}}) =>
          {
            let%opt_wrap {stamp: astamp} =
              attributes |> List.find_opt(a => a.aname.txt == name);
            addReference(astamp, nameLoc);
            LocalReference(stamp, Attribute(name));
          }
          |? NotFound
        | `Global(moduleName, path) =>
          addExternalReference(moduleName, path, Attribute(name), nameLoc);
          GlobalReference(moduleName, path, Attribute(name));
        | _ => NotFound
        };
      addLocation(nameLoc, Typed(lbl_res, locType));
    | _ => ()
    };
  };

  let addForRecord = (recordType, items) => {
    switch (Shared.dig(recordType).desc) {
    | Tconstr(path, _args, _memo) =>
      let t = getTypeAtPath(path);
      items
      |> List.iter((({Asttypes.txt, loc}, {Types.lbl_res}, _)) => {
           /* let name = Longident.last(txt); */

           let (name, typeLident) = handleConstructor(path, txt);
           maybeAddUse(path, typeLident, loc, Attribute(name));

           let nameLoc = Utils.endOfLocation(loc, String.length(name));
           let locType =
             switch (t) {
             | `Local({stamp, item: {kind: Record(attributes)}}) =>
               {
                 let%opt_wrap {stamp: astamp} =
                   attributes |> List.find_opt(a => a.aname.txt == name);
                 addReference(astamp, nameLoc);
                 LocalReference(stamp, Attribute(name));
               }
               |? NotFound
             | `Global(moduleName, path) =>
               addExternalReference(
                 moduleName,
                 path,
                 Attribute(name),
                 nameLoc,
               );
               GlobalReference(moduleName, path, Attribute(name));
             | _ => NotFound
             };
           addLocation(nameLoc, Typed(lbl_res, locType));
         });
    | _ => ()
    };
  };

  let addForConstructor =
      (constructorType, {Asttypes.txt, loc}, {Types.cstr_name}) => {
    switch (Shared.dig(constructorType).desc) {
    | Tconstr(path, _args, _memo) =>
      /* let name = Longident.last(txt); */

      let (name, typeLident) = handleConstructor(path, txt);
      maybeAddUse(path, typeLident, loc, Constructor(name));

      let nameLoc = Utils.endOfLocation(loc, String.length(name));
      let t = getTypeAtPath(path);
      let locType =
        switch (t) {
        | `Local({stamp, item: {kind: Variant(constructors)}}) =>
          {
            let%opt_wrap {stamp: cstamp} =
              constructors |> List.find_opt(c => c.cname.txt == cstr_name);
            addReference(cstamp, nameLoc);
            LocalReference(stamp, Constructor(name));
          }
          |? NotFound
        | `Global(moduleName, path) =>
          addExternalReference(moduleName, path, Constructor(name), nameLoc);
          GlobalReference(moduleName, path, Constructor(name));
        | _ => NotFound
        };
      addLocation(nameLoc, Typed(constructorType, locType));
    | _ => ()
    };
  };

  let currentScopeExtent = () =>
    if (Collector.scopeExtent^ == []) {
      Location.none;
    } else {
      List.hd(Collector.scopeExtent^);
    };
  let addScopeExtent = loc =>
    Collector.scopeExtent := [loc, ...Collector.scopeExtent^];
  let popScopeExtent = () =>
    if (List.length(Collector.scopeExtent^) > 1) {
      Collector.scopeExtent := List.tl(Collector.scopeExtent^);
    };

  let rec addForLongident = (top, path: Path.t, txt: Longident.t, loc) =>
    if (!loc.Location.loc_ghost) {
      let idLength =
        String.length(String.concat(".", Longident.flatten(txt)));
      let reportedLength = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum;
      let isPpx = idLength != reportedLength;
      if (isPpx) {
        switch (top) {
        | Some((t, tip)) => addForPath(path, txt, loc, t, tip)
        | None => addForPathParent(path, loc)
        };
      } else {
        let l =
          Utils.endOfLocation(loc, String.length(Longident.last(txt)));
        switch (top) {
        | Some((t, tip)) => addForPath(path, txt, l, t, tip)
        | None => addForPathParent(path, l)
        };
        switch (path, txt) {
        | (Pdot(pinner, _pname, _), Ldot(inner, name)) =>
          addForLongident(
            None,
            pinner,
            inner,
            Utils.chopLocationEnd(loc, String.length(name) + 1),
          )
        | (Pident(_), Lident(_)) => ()
        | _ => ()
        };
      };
    };

  let rec handle_module_expr = expr =>
    switch (expr) {
    | Tmod_constraint(expr, _, _, _) => handle_module_expr(expr.mod_desc)
    | Tmod_ident(path, {txt, loc}) =>
      Log.log("Ident!! " ++ String.concat(".", Longident.flatten(txt)));
      maybeAddUse(path, txt, loc, Module);
      addForLongident(None, path, txt, loc);
    | Tmod_functor(_ident, _argName, _maybeType, resultExpr) =>
      handle_module_expr(resultExpr.mod_desc)
    | Tmod_apply(obj, arg, _) =>
      handle_module_expr(obj.mod_desc);
      handle_module_expr(arg.mod_desc);
    | _ => ()
    };

  open Typedtree;
  include TypedtreeIter.DefaultIteratorArgument;
  let enter_structure_item = item =>
    switch (item.str_desc) {
    | Tstr_attribute((
        {Asttypes.txt: "ocaml.explanation", loc},
        PStr([
          {
            pstr_desc:
              Pstr_eval(
                {pexp_desc: Pexp_constant(Pconst_string(doc, _))},
                _,
              ),
          },
        ]),
      )) =>
      addLocation(loc, Explanation(doc))
    | Tstr_include({incl_mod: expr}) => handle_module_expr(expr.mod_desc)
    | Tstr_module({mb_expr}) => handle_module_expr(mb_expr.mod_desc)
    | Tstr_open({open_path, open_txt: {txt, loc} as l}) =>
      /* Log.log("Have an open here"); */
      maybeAddUse(open_path, txt, loc, Module);
      let tracker = {
        path: open_path,
        loc,
        ident: l,
        used: [],
        extent: {
          loc_ghost: true,
          loc_start: loc.loc_end,
          loc_end: currentScopeExtent().loc_end,
        },
      };
      addForLongident(None, open_path, txt, loc);
      Hashtbl.replace(Collector.extra.opens, loc, tracker);
    | _ => ()
    };

  let enter_structure = ({str_items}) =>
    if (str_items != []) {
      let first = List.hd(str_items);
      let last = List.nth(str_items, List.length(str_items) - 1);

      let extent = {
        Location.loc_ghost: true,
        loc_start: first.str_loc.loc_start,
        loc_end: last.str_loc.loc_end,
      };

      addScopeExtent(extent);
    };

  let leave_structure = str =>
    if (str.str_items != []) {
      popScopeExtent();
    };

  let enter_signature_item = item =>
    switch (item.sig_desc) {
    | Tsig_value({val_id, val_loc, val_name: name, val_desc, val_attributes}) =>
      let stamp = Ident.binding_time(val_id);
      if (!Hashtbl.mem(Collector.file.stamps.values, stamp)) {
        let declared =
          ProcessAttributes.newDeclared(
            ~name,
            ~stamp,
            ~extent=val_loc,
            ~scope={
              loc_ghost: true,
              loc_start: val_loc.loc_end,
              loc_end: currentScopeExtent().loc_end,
            },
            ~modulePath=NotVisible,
            ~processDoc=x => x,
            ~item=val_desc.ctyp_type,
            false,
            val_attributes,
          );
        Hashtbl.add(Collector.file.stamps.values, stamp, declared);
        addReference(stamp, name.loc);
        addLocation(
          name.loc,
          Typed(val_desc.ctyp_type, Definition(stamp, Value)),
        );
      };
    | _ => ()
    };

  let enter_core_type = ({ctyp_type, ctyp_desc}) => {
    switch (ctyp_desc) {
    | Ttyp_constr(path, {txt, loc}, _args) =>
      /* addForPath(path, txt, loc, Shared.makeFlexible(ctyp_type), Type) */
      addForLongident(Some((ctyp_type, Type)), path, txt, loc)
    | _ => ()
    };
  };

  let enter_pattern = ({pat_desc, pat_loc, pat_type, pat_attributes}) => {
    let addForPattern = (stamp, name) =>
      if (!Hashtbl.mem(Collector.file.stamps.values, stamp)) {
        let declared =
          ProcessAttributes.newDeclared(
            ~name,
            ~stamp,
            ~scope={
              loc_ghost: true,
              loc_start: pat_loc.loc_end,
              loc_end: currentScopeExtent().loc_end,
            },
            ~modulePath=NotVisible,
            ~extent=pat_loc,
            ~processDoc=x => x,
            ~item=pat_type,
            false,
            pat_attributes,
          );
        Hashtbl.add(Collector.file.stamps.values, stamp, declared);
        addReference(stamp, name.loc);
        addLocation(name.loc, Typed(pat_type, Definition(stamp, Value)));
      };
    /* Log.log("Entering pattern " ++ Utils.showLocation(pat_loc)); */
    switch (pat_desc) {
    | Tpat_record(items, _) => addForRecord(pat_type, items)
    | Tpat_construct(lident, constructor, _) =>
      addForConstructor(pat_type, lident, constructor)
    | Tpat_alias(_inner, ident, name) =>
      let stamp = Ident.binding_time(ident);
      addForPattern(stamp, name);
    | Tpat_var(ident, name) =>
      /* Log.log("Pattern " ++ name.txt); */
      let stamp = Ident.binding_time(ident);
      addForPattern(stamp, name);
    | _ => ()
    };
    if (Collector.allLocations) {
      addLocation(pat_loc, Typed(pat_type, NotFound));
    };
  };

  let enter_expression = expression => {
    expression.exp_extra
    |> List.iter(((e, eloc, _)) =>
         switch (e) {
         | Texp_open(_, path, ident, _) =>
           Hashtbl.add(
             extra.opens,
             eloc,
             {path, ident, loc: eloc, extent: expression.exp_loc, used: []},
           )
         | _ => ()
         }
       );
    switch (expression.exp_desc) {
    /* | Texp_apply({exp_desc: Pexp_ident(_, {txt: Ldot(Lident("ReasonReact"), "element")})}, [(_, {exp_desc: Pexp_apply({exp_desc: Pexp_ident(_, {txt})}, _)})]) =>{

       } */
    | Texp_ident(path, {txt, loc}, {val_type}) =>
      addForLongident(Some((val_type, Value)), path, txt, loc)
    | Texp_record({fields}) =>
      addForRecord(
        expression.exp_type,
        fields
        |> Array.to_list
        |> Utils.filterMap(((desc, item)) => {
             switch (item) {
             | Overridden(loc, _) => Some((loc, desc, ()))
             | _ => None
             }
           }),
      )
    | Texp_constant(constant) =>
      addLocation(expression.exp_loc, Constant(constant))
    /* Skip unit and list literals */
    | Texp_construct({txt: Lident("()" | "::"), loc}, _, _args)
        when loc.loc_end.pos_cnum - loc.loc_start.pos_cnum != 2 =>
      ()
    | Texp_construct(lident, constructor, _args) =>
      addForConstructor(expression.exp_type, lident, constructor)
    | Texp_field(inner, lident, label_description) =>
      addForField(inner.exp_type, label_description, lident)
    | Texp_let(_, _, _) =>
      addScopeExtent(
        expression.exp_loc,
        /* TODO this scope tracking won't work for recursive */
      )
    | Texp_function({cases}) =>
      switch (cases) {
      | [{c_rhs}] => addScopeExtent(c_rhs.exp_loc)
      | _ => ()
      }
    | _ => ()
    };
    if (Collector.allLocations) {
      addLocation(expression.exp_loc, Typed(expression.exp_type, NotFound));
    };
  };

  let leave_expression = expression => {
    switch (expression.exp_desc) {
    | Texp_let(_isrec, _bindings, _expr) => popScopeExtent()
    | Texp_function({cases}) =>
      switch (cases) {
      | [_] => popScopeExtent()
      | _ => ()
      }
    | _ => ()
    };
  };
};

let forFile = (~file) => {
  let extra = initExtra();
  let addLocation = (loc, ident) =>
    extra.locations = [(loc, ident), ...extra.locations];
  let addReference = (stamp, loc) =>
    Hashtbl.replace(
      extra.internalReferences,
      stamp,
      [
        loc,
        ...Hashtbl.mem(extra.internalReferences, stamp)
             ? Hashtbl.find(extra.internalReferences, stamp) : [],
      ],
    );
  file.stamps.modules
  |> Hashtbl.iter((stamp, d) => {
       addLocation(d.name.loc, LModule(Definition(stamp, Module)));
       addReference(stamp, d.name.loc);
     });
  file.stamps.values
  |> Hashtbl.iter((stamp, d) => {
       addLocation(d.name.loc, Typed(d.item, Definition(stamp, Value)));
       addReference(stamp, d.name.loc);
     });
  file.stamps.types
  |> Hashtbl.iter((stamp, d) => {
       addLocation(
         d.name.loc,
         TypeDefinition(d.name.txt, d.item.Type.decl, stamp),
       );
       addReference(stamp, d.name.loc);
       switch (d.item.Type.kind) {
       | Record(labels) =>
         labels
         |> List.iter(({stamp, aname, typ}) => {
              addReference(stamp, aname.loc);
              addLocation(
                aname.loc,
                Typed(typ, Definition(d.stamp, Attribute(aname.txt))),
              );
            })
       | Variant(constructos) =>
         constructos
         |> List.iter(({stamp, cname}) => {
              addReference(stamp, cname.loc);
              let t = {
                Types.id: 0,
                level: 0,
                desc:
                  Tconstr(
                    Path.Pident({Ident.stamp, name: d.name.txt, flags: 0}),
                    [],
                    ref(Types.Mnil),
                  ),
              };
              addLocation(
                cname.loc,
                Typed(t, Definition(d.stamp, Constructor(cname.txt))),
              );
            })
       | _ => ()
       };
     });

  extra;
};

let forItems = (~file, ~allLocations, items, parts) => {
  let extra = forFile(~file);

  let extent = ProcessCmt.itemsExtent(items);
  let extent = {
    ...extent,
    loc_end: {
      ...extent.loc_end,
      pos_lnum: extent.loc_end.pos_lnum + 1000000,
      pos_cnum: extent.loc_end.pos_cnum + 100000000,
    },
  };

  /* TODO look through parts and extend the extent */

  module Iter =
    TypedtreeIter.MakeIterator(
      (
        F({
          let scopeExtent = ref([extent]);
          let extra = extra;
          let file = file;
          let allLocations = allLocations;
        })
      ),
    );

  List.iter(Iter.iter_structure_item, items);
  /* Log.log("Parts " ++ string_of_int(Array.length(parts))); */

  parts
  |> Array.iter(part =>
       switch (part) {
       | Cmt_format.Partial_signature(str) => Iter.iter_signature(str)
       | Partial_signature_item(str) => Iter.iter_signature_item(str)
       | Partial_expression(expression) => Iter.iter_expression(expression)
       | Partial_pattern(pattern) => Iter.iter_pattern(pattern)
       | Partial_class_expr(class_expr) => Iter.iter_class_expr(class_expr)
       | Partial_module_type(module_type) =>
         Iter.iter_module_type(module_type)
       | Partial_structure(_)
       | Partial_structure_item(_) => ()
       }
     );

  extra;
};

let forCmt = (~file, ~allLocations, {cmt_annots}: Cmt_format.cmt_infos) =>
  switch (cmt_annots) {
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
    forItems(~file, ~allLocations, items, parts);
  | Implementation(structure) =>
    forItems(~file, ~allLocations, structure.str_items, [||])
  | Partial_interface(_)
  | Interface(_) =>
    /** TODO actually process signature items */
    forItems(~file, ~allLocations, [], [||])
  | _ => forItems(~file, ~allLocations, [], [||])
  };
