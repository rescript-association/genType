/***************************************************************************/
/*                                                                         */
/*   Copyright (c) 2014-2016 LexiFi SAS. All rights reserved.              */
/*                                                                         */
/*   This source code is licensed under the MIT License                    */
/*   found in the LICENSE file at the root of this source tree             */
/*                                                                         */
/***************************************************************************/

let reportUnderscore = ref(false);

let active = Sys.getenv_opt("Global") != None;

let write = Sys.getenv_opt("Write") != None;

let verbose = Sys.getenv_opt("Verbose") != None;

let deadAnnotation = "dead";

/********   ATTRIBUTES   ********/
module LocSet =
  Set.Make({
    type t = Lexing.position;
    let compare = compare;
  });

module LocHash = {
  include Hashtbl.Make({
    type t = Lexing.position;

    let hash = x => {
      let s = Filename.basename(x.Lexing.pos_fname);
      Hashtbl.hash((x.Lexing.pos_cnum, s));
    };

    let equal = (x: t, y) => x == y;
  });

  let find_set = (h, k) =>
    try(find(h, k)) {
    | Not_found => LocSet.empty
    };

  let add_set = (h, k, v) => {
    let l = find_set(h, k);
    replace(h, k, LocSet.add(v, l));
  };

  let merge_set = (h1, k1, h2, k2) => {
    let l1 = find_set(h1, k1);
    let l2 = find_set(h2, k2);
    replace(h1, k1, LocSet.union(l1, l2));
  };
};

type decs = Hashtbl.t(Lexing.position, string);
let decs: decs = (Hashtbl.create(256): decs); /* all exported value declarations */

let incl: decs = (Hashtbl.create(256): decs); /* all exported value declarations */

let references: LocHash.t(LocSet.t) = (
  LocHash.create(256): LocHash.t(LocSet.t)
); /* all value references */

let fields: Hashtbl.t(string, Lexing.position) = (
  Hashtbl.create(256): Hashtbl.t(string, Lexing.position)
); /* link from fields (record/variant) paths and locations */

let last_loc = ref(Lexing.dummy_pos); /* helper to diagnose occurrences of Location.none in the typedtree */
let current_src = ref("");

let mods: ref(list(string)) = (ref([]): ref(list(string))); /* module path */

let none_ = "_none_";
let include_ = "*include*";

/********   HELPERS   ********/

let getModuleName = fn => fn |> Paths.getModuleName |> ModuleName.toString;

let is_ghost = loc =>
  loc.Lexing.pos_lnum <= 0
  || loc.Lexing.pos_cnum
  - loc.Lexing.pos_bol < 0
  || loc.Lexing.pos_fname == none_
  || loc.Lexing.pos_fname == "";

let check_underscore = name => reportUnderscore^ || name.[0] != '_';

let hashtbl_find_list = (hashtbl, key) => Hashtbl.find_all(hashtbl, key);

let hashtbl_add_to_list = (hashtbl, key, elt) =>
  Hashtbl.add(hashtbl, key, elt);

let hashtbl_add_unique_to_list = (hashtbl, key, elt) =>
  if (!List.mem(elt, hashtbl_find_list(hashtbl, key))) {
    Hashtbl.add(hashtbl, key, elt);
  };

let rec hashtbl_remove_list = (hashtbl, key) =>
  if (Hashtbl.mem(hashtbl, key)) {
    Hashtbl.remove(hashtbl, key);
    hashtbl_remove_list(hashtbl, key);
  };

let hashtbl_replace_list = (hashtbl, key, l) => {
  hashtbl_remove_list(hashtbl, key);
  List.iter(elt => hashtbl_add_to_list(hashtbl, key, elt), l);
};

let hashtbl_merge_unique_list = (tbl1, key1, tbl2, key2) =>
  List.iter(
    elt => hashtbl_add_unique_to_list(tbl1, key1, elt),
    hashtbl_find_list(tbl2, key2),
  );

let find_path = (fn, ~sep='/', l) =>
  List.find(
    path => {
      let lp = String.length(path)
      and lf = String.length(fn);
      (lp > lf && path.[lp - lf - 1] == sep || lp == lf)
      && String.sub(path, lp - lf, lf) == fn;
    },
    l,
  );

/* Location printer: `filename:line: ' */
let prloc = (~printCol=false, loc: Lexing.position) => {
  let file = loc.Lexing.pos_fname;
  let line = loc.Lexing.pos_lnum;
  let col = loc.Lexing.pos_cnum - loc.Lexing.pos_bol;
  print_string(file);
  print_char(':');
  print_int(line);
  if (printCol) {
    print_char(':');
    print_int(col);
  } else {
    print_string(": ");
  };
};

/********   PROCESSING  ********/

let export = (~sep=".", path, u, stock: decs, id, loc) => {
  let value =
    String.concat(".", List.rev_map(Ident.name, path))
    ++ sep
    ++ id.Ident.name;

  /* a .cmi file can contain locations from other files.
       For instance:
           module M : Set.S with type elt = int
       will create value definitions whose location is in set.mli
     */
  if (!loc.Location.loc_ghost
      && (
        u == getModuleName(loc.Location.loc_start.Lexing.pos_fname)
        || u === include_
      )
      && check_underscore(id.Ident.name)) {
    hashtbl_add_to_list(stock, loc.Location.loc_start, value);
  };
};

/**** REPORTING ****/

/* Faster than 'List.length l = len' when len < List.length l; same speed otherwise*/
let rec check_length = len =>
  fun
  | [] => len == 0
  | [_, ...l] when len > 0 => check_length(len - 1, l)
  | _ => false;

let pathWithoutHead = path => {
  let rec cutFromNextDot = (s, pos) =>
    if (pos == String.length(s)) {
      s;
    } else if (s.[pos] == '.') {
      String.sub(s, pos + 1, String.length(s) - pos - 1);
    } else {
      cutFromNextDot(s, pos + 1);
    };
  cutFromNextDot(path, 0);
};

type item = {
  loc: Lexing.position,
  path: string,
};

let compareItems = ({path: path1, loc: loc1}, {path: path2, loc: loc2}) =>
  compare((loc1, path1), (loc2, path2));

let report = (~dontReportDead=_ => false, ~onItem, decs: decs) => {
  let folder = (loc, path, items) => {
    switch (loc |> LocHash.find_set(references)) {
    | referencesToLoc when !(loc |> dontReportDead) =>
      if (referencesToLoc |> LocSet.cardinal == 0) {
        [{loc, path: pathWithoutHead(path)}, ...items];
      } else {
        if (verbose) {
          GenTypeCommon.logItem(
            "%s: %d references\n",
            path,
            referencesToLoc |> LocSet.cardinal,
          );
        };
        items;
      }
    | _ => items
    | exception Not_found => items
    };
  };

  Hashtbl.fold(folder, decs, [])
  |> List.fast_sort(compareItems)
  |> List.iter(({loc, path}) => {onItem(loc, path)});
};