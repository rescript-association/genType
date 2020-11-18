let tryReadCmt = cmt =>
  if (!Files.exists(cmt)) {
    Error("Cmt file does not exist " ++ cmt);
  } else {
    switch (Cmt_format.read_cmt(cmt)) {
    | exception (Cmi_format.Error(err)) =>
      Error(
        "Failed to load "
        ++ cmt
        ++ " as a cmt w/ ocaml version "
        ++ "406"
        ++ ", error: "
        ++ {
          Cmi_format.report_error(Format.str_formatter, err);
          Format.flush_str_formatter();
        },
      )
    | exception err =>
      Error(
        "Invalid cmt format "
        ++ cmt
        ++ " - probably wrong ocaml version, expected "
        ++ Config.version
        ++ " : "
        ++ Printexc.to_string(err),
      )
    | x => Ok(x)
    };
  };

/** TODO move to the Process_ stuff */
let rec dig = typ =>
  switch (typ.Types.desc) {
  | Types.Tlink(inner) => dig(inner)
  | Types.Tsubst(inner) => dig(inner)
  | Types.Tpoly(inner, _) => dig(inner)
  | _ => typ
  };

let digConstructor = expr => {
  let expr = dig(expr);
  switch (expr.desc) {
  | Tconstr(path, _args, _memo) => Some(path)
  | _ => None
  };
};

let rec variableKind = t =>
  switch (t.Types.desc) {
  | Tlink(t) => variableKind(t)
  | Tsubst(t) => variableKind(t)
  | Tarrow(_) => SharedTypes.Function
  | Ttuple(_) => Array
  | Tconstr(_) => Variable
  | Tobject(_) => Object
  | Tnil => Null
  | Tvariant(_) => EnumMember
  | Tpoly(_) => EnumMember
  | Tpackage(_) => Module
  | _ => Variable
  };

let declarationKind = t =>
  switch (t.Types.type_kind) {
  | Type_open
  | Type_abstract => SharedTypes.TypeParameter
  | Type_record(_) => Interface
  | Type_variant(_) => Enum
  };

let declToString = (name, t) =>
  PrintType.print_decl(name, name, t) |> PrintType.prettyString;

let labelToString = label =>
  switch (label) {
  | Asttypes.Nolabel => ""
  | Optional(label)
  | Labelled(label) => label
  };

let cacheTypeToString = ref(false);
let typeTbl = Hashtbl.create(1);

let typeToString = (t: Types.type_expr) => {
  switch (cacheTypeToString^ ? Hashtbl.find_opt(typeTbl, (t.id, t)) : None) {
  | None =>
    let s = PrintType.print_expr(t) |> PrintType.prettyString(~width=40);
    Hashtbl.replace(typeTbl, (t.id, t), s);
    s;
  | Some(s) => s
  };
};

let rec getArguments = t =>
  switch (t.Types.desc) {
  | Types.Tsubst(t)
  | Tlink(t) => getArguments(t)
  | Tarrow(label, argt, res, _) =>
    let args = getArguments(res);
    [(labelToString(label), argt), ...args];
  | _ => []
  };
