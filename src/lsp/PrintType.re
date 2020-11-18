let rescript = ref(true);

let rec dig = typ =>
  switch (typ.Types.desc) {
  | Types.Tlink(inner) => dig(inner)
  | Types.Tsubst(inner) => dig(inner)
  | Types.Tpoly(inner, _) => dig(inner)
  | _ => typ
  };

let rec collectArgs = (coll, typ) =>
  switch (typ.Types.desc) {
  | Types.Tarrow(label, arg, result, _) =>
    collectArgs([(label, arg), ...coll], result)
  | Tlink(inner) => collectArgs(coll, inner)
  | Tsubst(inner) => collectArgs(coll, inner)
  | _ => (coll, typ)
  };

let break = Pretty.line("");
let space = Pretty.line(" ");
let dedent = Pretty.back(2, "");

let str = (~len=?, s) => Pretty.text(~len?, s);
let (@!) = Pretty.append;

let sepdList = (sep, items, printItem) => {
  let rec recur = items =>
    switch (items) {
    | [] => Pretty.empty
    | [one] => printItem(one)
    | [one, ...more] =>
      let l = printItem(one);
      l @! sep @! recur(more);
    };
  recur(items);
};

let commadList = (printItem, items) => {
  sepdList(str(",") @! space, items, printItem);
};

let indentGroup = doc => Pretty.indent(2, Pretty.group(doc));

let tupleList = (items, printItem) => {
  str("(")
  @! indentGroup(break @! commadList(printItem, items) @! dedent)
  @! str(")");
};

let typeConstr = (items, printItem) =>
  if (rescript^) {
    str("<")
    @! indentGroup(break @! commadList(printItem, items) @! dedent)
    @! str(">");
  } else {
    tupleList(items, printItem);
  };

let showArgs = (loop, args) => {
  str("(")
  @! indentGroup(
       break
       @! commadList(
            ((label, typ)) => {
              switch (label) {
              | Asttypes.Nolabel => loop(typ)
              | Labelled(label)
              | Optional(label) => str("~" ++ label ++ ": ") @! loop(typ)
              }
            },
            args,
          )
       @! dedent,
     )
  @! str(")");
};

type namer = {
  reset: unit => unit,
  get: Types.type_expr => string,
};
let makeNamer = () => {
  let alphabet = "abcdefghijklmnopqrstuvwxyz";
  let latest = ref(0);
  let names = Hashtbl.create(10);
  let newName = () => {
    let i = latest^;
    latest := i + 1;
    let num = i > 25 ? "t" ++ string_of_int(i) : String.sub(alphabet, i, 1);
    "'" ++ num;
  };
  let get = t =>
    try(Hashtbl.find(names, t)) {
    | Not_found =>
      let name = newName();
      Hashtbl.replace(names, t, name);
      name;
    };
  let reset = () => {
    latest := 0;
    Hashtbl.clear(names);
  };
  {get, reset};
};

let namer = makeNamer();

let ident = id => str(Ident.name(id));

let rec print_expr = (~depth=0, typ) => {
  /* Log.log("print_expr"); */
  let innerExpr = print_expr(~depth=depth + 1);
  if (depth > 20) {
    str("Too deep");
  } else {
    Types.(
      switch (typ.desc) {
      | Tvar(None) => str(namer.get(typ))
      | Tvar(Some(s)) => str("'" ++ s)
      | Tarrow(label, arg, result, _) =>
        let (args, result) = collectArgs([(label, arg)], result);
        let args = List.rev(args);
        (
          switch (args) {
          | [(Nolabel, typ)] =>
            switch (dig(typ)) {
            | {desc: Ttuple(_)} => showArgs(innerExpr, args)
            | _ => innerExpr(typ)
            }
          | _ => showArgs(innerExpr, args)
          }
        )
        @! str(" => ")
        @! innerExpr(result);
      | Ttuple(items) => tupleList(items, innerExpr)
      | Tconstr(path, args, _) =>
        print_path(path)
        @! (
          switch (args) {
          | [] => Pretty.empty
          | args => typeConstr(args, innerExpr)
          }
        )
      | Tlink(inner) => innerExpr(inner)
      | Tsubst(inner) => innerExpr(inner)
      | Tnil => str("(no type)")
      | Tvariant({row_fields}) =>
        str("[")
        @! indentGroup(
             break
             @! (List.length(row_fields) <= 1 ? str("| ") : str(" "))
             @! sepdList(
                  space @! str("| "), row_fields, ((label, row_field)) =>
                  switch (row_field) {
                  | Rpresent(None)
                  | Reither(_, [], _, _) => str("#" ++ label)
                  | Rpresent(Some(t))
                  | Reither(_, [t], _, _) =>
                    str("#" ++ label)
                    @! str("(")
                    @! innerExpr(t)
                    @! str(")")
                  | Reither(_)
                  | Rabsent => str("...")
                  }
                ),
           )
        @! str("]")
        @! break
      | Tfield(_, _, _, _)
      | Tunivar(_)
      | Tpoly(_, _)
      | Tpackage(_, _, _)
      | Tobject(_, _) =>
        let txt = {
          try(Printtyp.type_expr(Format.str_formatter, typ)) {
          | _ => Format.fprintf(Format.str_formatter, "Unable to print type")
          };
          Format.flush_str_formatter();
        };
        str(txt);
      }
    );
  };
}

and print_path = path =>
  switch (path) {
  | Path.Pident(id) => ident(id)
  | Pdot(path, name, _) => print_path(path) @! str("." ++ name)
  | Papply(_, _) => str("<apply>")
  };

let print_constructor = (loop, {Types.cd_id, cd_args, cd_res}) => {
  let name = Ident.name(cd_id);
  str(name)
  @! (
    switch (cd_args) {
    | Cstr_tuple([]) => Pretty.empty
    | Cstr_record(_) => str("{...printing not supported...}")
    | Cstr_tuple(args) => typeConstr(args, loop)
    }
  )
  @! (
    switch (cd_res) {
    | None => Pretty.empty
    | Some(typ) => str(": ") @! loop(typ)
    }
  );
};

let print_attr = ({Types.ld_id, ld_mutable, ld_type}) => {
  (
    switch (ld_mutable) {
    | Asttypes.Immutable => Pretty.empty
    | Mutable => str("mut ")
    }
  )
  @! ident(ld_id)
  @! str(": ")
  @! print_expr(ld_type);
};

let print_decl = (realName, name, decl) => {
  Types.(
    str("type ")
    @! str(~len=String.length(realName), name)
    @! (
      switch (decl.type_params) {
      | [] => Pretty.empty
      | args => typeConstr(args, print_expr)
      }
    )
    @! (
      switch (decl.type_kind) {
      | Type_abstract => Pretty.empty
      | Type_open => str(" = ..")
      | Type_record(labels, _representation) =>
        str(" = {")
        @! indentGroup(break @! commadList(print_attr, labels) @! dedent)
        @! str("}")
      | Type_variant(constructors) =>
        str(" = ")
        @! indentGroup(
             break
             @! str("| ")
             @! sepdList(
                  space @! str("| "),
                  constructors,
                  print_constructor(print_expr),
                ),
           )
        @! break
      }
    )
    @! (
      switch (decl.type_manifest) {
      | None => Pretty.empty
      | Some(manifest) => str(" = ") @! print_expr(manifest)
      }
    )
  );
};

let prettyString = (~width=60, doc) => {
  namer.reset();
  let buffer = Buffer.create(100);
  Pretty.print(
    ~width,
    ~output=text => Buffer.add_string(buffer, text),
    ~indent=
      num => {
        Buffer.add_string(buffer, "\n");
        for (_ in 1 to num) {
          Buffer.add_char(buffer, ' ');
        };
      },
    doc,
  );
  Buffer.contents(buffer);
};
