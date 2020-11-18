type kinds =
  | Function
  | Array
  | Variable
  | Object
  | Null
  | EnumMember
  | Module
  | Enum
  | Interface
  | TypeParameter;

type filePath = string;
type paths =
  | Impl(filePath, option(filePath))
  | Intf(filePath, filePath)
  // .cm(t)i, .mli, .cmt, .rei
  | IntfAndImpl(filePath, filePath, filePath, filePath);

open Infix;
let showPaths = paths =>
  switch (paths) {
  | Impl(cmt, src) => Printf.sprintf("Impl(%s, %s)", cmt, src |? "nil")
  | Intf(cmti, src) => Printf.sprintf("Intf(%s, %s)", cmti, src)
  | IntfAndImpl(cmti, srci, cmt, src) =>
    Printf.sprintf("IntfAndImpl(%s, %s, %s, %s)", cmti, srci, cmt, src)
  };

let getSrc = p =>
  switch (p) {
  | Impl(_, s) => s
  | Intf(_, s)
  | IntfAndImpl(_, s, _, _) => Some(s)
  };

let getCmt = (~interface=true, p) =>
  switch (p) {
  | Impl(c, _)
  | Intf(c, _) => c
  | IntfAndImpl(cint, _, cimpl, _) => interface ? cint : cimpl
  };

/* TODO maybe track the loc's of these things */
type visibilityPath =
  | File(string, string)
  | NotVisible
  | IncludedModule(Path.t, visibilityPath)
  | ExportedModule(string, visibilityPath);

type declared('t) = {
  name: Location.loc(string),
  extentLoc: Location.t,
  scopeLoc: Location.t,
  stamp: int,
  modulePath: visibilityPath,
  exported: bool,
  docstring: option(string),
  item: 't,
  /* TODO maybe add a uri? */
  /* scopeType: scope, */
  /* scopeStart: (int, int), */
};

let emptyDeclared = name => {
  name: Location.mknoloc(name),
  extentLoc: Location.none,
  scopeLoc: Location.none,
  stamp: 0,
  modulePath: NotVisible,
  exported: false,
  docstring: None,
  item: (),
};

type attribute = {
  stamp: int,
  aname: Location.loc(string),
  typ: Types.type_expr,
};

type constructor = {
  stamp: int,
  cname: Location.loc(string),
  args: list((Types.type_expr, Location.t)),
  res: option(Types.type_expr),
};

module Type = {
  type kind =
    | Abstract(option((Path.t, list(Types.type_expr))))
    | Open
    | Tuple(list(Types.type_expr))
    | Record(list(attribute))
    | Variant(list(constructor));

  type t = {
    kind,
    decl: Types.type_declaration,
  };
};

/* type scope =
   | File
   | Switch
   | Module
   | Let
   | LetRec; */

let isVisible = declared =>
  declared.exported
  && {
    let rec loop = v =>
      switch (v) {
      | File(_) => true
      | NotVisible => false
      | IncludedModule(_, inner) => loop(inner)
      | ExportedModule(_, inner) => loop(inner)
      };
    loop(declared.modulePath);
  };

type namedMap('t) = Hashtbl.t(string, 't);
type namedStampMap = namedMap(int);

type exported = {
  types: namedStampMap,
  values: namedStampMap,
  modules: namedStampMap,
  /* constructors: namedStampMap, */
  /* classes: namedStampMap,
     classTypes: namedStampMap, */
};
let initExported = () => {
  types: Hashtbl.create(10),
  values: Hashtbl.create(10),
  modules: Hashtbl.create(10),
  /* constructors: Hashtbl.create(10), */
};
type moduleItem =
  | MValue(Types.type_expr)
  | MType(Type.t)
  | Module(moduleKind)
and moduleContents = {
  exported,
  mutable topLevel: list(declared(moduleItem)),
}
and moduleKind =
  | Ident(Path.t)
  | Structure(moduleContents);

type stampMap('t) = Hashtbl.t(int, 't);

type stamps = {
  types: stampMap(declared(Type.t)),
  values: stampMap(declared(Types.type_expr)),
  modules: stampMap(declared(moduleKind)),
  constructors: stampMap(declared(constructor)),
};

let initStamps = () => {
  types: Hashtbl.create(10),
  values: Hashtbl.create(10),
  modules: Hashtbl.create(10),
  constructors: Hashtbl.create(10),
};

type file = {
  uri: string,
  stamps,
  moduleName: string,
  contents: moduleContents,
};

let emptyFile = (moduleName, uri) => {
  uri,
  stamps: initStamps(),
  moduleName,
  contents: {
    exported: initExported(),
    topLevel: [],
  },
};

type tip =
  | Value
  | Type
  | Attribute(string)
  | Constructor(string)
  | Module;

let tipToString = tip =>
  switch (tip) {
  | Value => "Value"
  | Type => "Type"
  | Attribute(a) => "Attribute(" ++ a ++ ")"
  | Constructor(a) => "Constructor(" ++ a ++ ")"
  | Module => "Module"
  };

type path =
  | Tip(string)
  | Nested(string, path);

let rec pathToString = path =>
  switch (path) {
  | Tip(name) => name
  | Nested(name, inner) => name ++ "." ++ pathToString(inner)
  };

let rec pathFromVisibility = (visibilityPath, current) =>
  switch (visibilityPath) {
  | File(_) => Some(current)
  | IncludedModule(_, inner) => pathFromVisibility(inner, current)
  | ExportedModule(name, inner) =>
    pathFromVisibility(inner, Nested(name, current))
  | NotVisible => None
  };

let pathFromVisibility = (visibilityPath, tipName) =>
  pathFromVisibility(visibilityPath, Tip(tipName));

type locKind =
  | LocalReference(int, tip)
  | GlobalReference(string, path, tip)
  | NotFound
  | Definition(int, tip);
let locKindToString = t =>
  switch (t) {
  | LocalReference(stamp, tip) =>
    Printf.sprintf("Local(%d, %s)", stamp, tipToString(tip))
  | GlobalReference(m, path, tip) =>
    Printf.sprintf(
      "Global(%s, %s, %s)",
      m,
      pathToString(path),
      tipToString(tip),
    )
  | Definition(stamp, tip) =>
    Printf.sprintf("Definition(%d, %s)", stamp, tipToString(tip))
  | NotFound => "NotFound"
  };
type loc =
  | Typed(Types.type_expr, locKind)
  | Constant(Asttypes.constant)
  | LModule(locKind)
  | TopLevelModule(string)
  | TypeDefinition(string, Types.type_declaration, int)
  | Explanation(string);

type openTracker = {
  path: Path.t,
  loc: Location.t,
  ident: Location.loc(Longident.t),
  extent: Location.t,
  mutable used: list((path, tip, Location.t)),
};

/** These are the bits of info that we need to make in-app stuff awesome */
type extra = {
  internalReferences: Hashtbl.t(int, list(Location.t)),
  externalReferences: Hashtbl.t(string, list((path, tip, Location.t))),
  mutable locations: list((Location.t, loc)),
  /* This is the "open location", like the location...
     or maybe the >> location of the open ident maybe */
  /* OPTIMIZE: using a stack to come up with this would cut the computation time of this considerably. */
  opens: Hashtbl.t(Location.t, openTracker),
};

type full = {
  extra,
  file,
};

let initExtra = () => {
  internalReferences: Hashtbl.create(10),
  externalReferences: Hashtbl.create(10),
  locations: [],
  opens: Hashtbl.create(10),
};

let initFull = (moduleName, uri) => {
  file: emptyFile(moduleName, uri),
  extra: initExtra(),
};

let hashList = h => Hashtbl.fold((a, b, c) => [(a, b), ...c], h, []);
