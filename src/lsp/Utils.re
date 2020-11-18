/*
   steal from OCaml stdlib
   https://github.com/ocaml/ocaml/blob/7c9c210884e1b46f21af5bb4dfab995bb3336cf7/stdlib/string.ml#L205-L214
 */
let split_on_char = (sep, s) => {
  open String;
  let r = ref([]);
  let j = ref(length(s));
  for (i in length(s) - 1 downto 0) {
    if (unsafe_get(s, i) == sep) {
      r := [sub(s, i + 1, j^ - i - 1), ...r^];
      j := i;
    };
  };
  [sub(s, 0, j^), ...r^];
};

let getFullLineOfPos = (pos, s) => {
  let left =
    switch (String.rindex_from(s, pos, '\n') + 1) {
    | pos => pos
    | exception Not_found => 0
    };
  let right =
    switch (String.index_from(s, pos, '\n') - 1) {
    | pos => pos
    | exception Not_found => String.length(s) - 1
    };
  String.sub(s, left, right - left);
};

let repeat = (length, s) => {
  let result = ref("");
  for (_ in 1 to length) {
    result := result^ ++ s;
  };
  result^;
};

let countLeading = (value, s) => {
  let length = String.length(s);
  let rec loop = (count, i) =>
    if (i < length && s.[i] == value) {
      loop(count + 1, i + 1);
    } else {
      count;
    };
  loop(0, 0);
};

let topLoc = fname => {
  Location.loc_start: {
    Lexing.pos_fname: fname,
    pos_lnum: 1,
    pos_bol: 0,
    pos_cnum: 0,
  },
  Location.loc_end: {
    Lexing.pos_fname: fname,
    pos_lnum: 1,
    pos_bol: 0,
    pos_cnum: 0,
  },
  loc_ghost: false,
};

let countTrailing = (value, s) => {
  let length = String.length(s);
  let rec loop = (count, i) =>
    if (i >= 0 && s.[i] == value) {
      loop(count + 1, i - 1);
    } else {
      count;
    };
  loop(0, length - 1);
};
/**
 * `startsWith(string, prefix)`
 * true if the string starts with the prefix
 */
let startsWith = (s, prefix) =>
  if (prefix == "") {
    true;
  } else {
    let p = String.length(prefix);
    p <= String.length(s) && String.sub(s, 0, p) == prefix;
  };

let endsWith = (s, suffix) =>
  if (suffix == "") {
    true;
  } else {
    let p = String.length(suffix);
    let l = String.length(s);
    p <= String.length(s) && String.sub(s, l - p, p) == suffix;
  };

let cmtLocFromVscode = ((line, col)) => (line + 1, col);

let splitLines = text => Str.split(Str.regexp_string("\n"), text);

let stripAnsii = text =>
  Str.global_replace(Str.regexp("\027\\[[0-9;]+m"), "", text);

let sliceToEnd = (s, start) => {
  let l = String.length(s);
  start <= l ? String.sub(s, start, l - start) : "";
};

let locWithinLoc = (inner, outer) => {
  Location.(
    inner.loc_start.pos_cnum >= outer.loc_start.pos_cnum
    && inner.loc_end.pos_cnum <= outer.loc_end.pos_cnum
  );
};

let toUri = path =>
  if (Sys.os_type == "Unix") {
    "file://" ++ path;
  } else {
    "file://"
    ++ (
      Str.global_replace(Str.regexp_string("\\"), "/", path)
      |> Str.substitute_first(
           Str.regexp("^\\([A-Z]\\):"),
           text => {
             let name = Str.matched_group(1, text);
             "/" ++ String.lowercase_ascii(name) ++ "%3A";
           },
         )
    );
  };

let parseWindowsUri = withoutScheme => {
  withoutScheme
  |> Str.substitute_first(
       Str.regexp("^/\\([a-z]\\)%3A"),
       text => {
         let name = Str.matched_group(1, text);
         String.uppercase_ascii(name) ++ ":";
       },
     )
  /* OCaml doesn't want to do a find & replace where the replacement is a single backslash. So this works */
  |> Str.split(Str.regexp_string("/"))
  |> String.concat({|\|});
};

let parseUri = uri => {
  let withoutPct = Uri.pct_decode(uri);
  if (startsWith(withoutPct, "file://")) {
    let withoutScheme = sliceToEnd(withoutPct, String.length("file://"));

    if (Sys.os_type == "Unix") {
      Some(withoutScheme);
    } else {
      Some(parseWindowsUri(withoutScheme));
    };
  } else {
    None;
  };
};

let endOfLocation = (loc, length) =>
  Location.{
    ...loc,
    loc_start: {
      ...loc.loc_end,
      pos_cnum: loc.loc_end.pos_cnum - length,
    },
  };

let chopLocationEnd = (loc, length) =>
  Location.{
    ...loc,
    loc_end: {
      ...loc.loc_end,
      pos_cnum: loc.loc_end.pos_cnum - length,
    },
  };

let chopPrefix = (s, prefix) => sliceToEnd(s, String.length(prefix));

/** An optional List.find */
let rec find = (fn, items) =>
  switch (items) {
  | [] => None
  | [one, ...rest] =>
    switch (fn(one)) {
    | None => find(fn, rest)
    | Some(x) => Some(x)
    }
  };

let joinLines = String.concat("\n");

let dedup = items => {
  let m = Hashtbl.create(List.length(items));
  items
  |> List.filter(a =>
       if (Hashtbl.mem(m, a)) {
         false;
       } else {
         Hashtbl.add(m, a, ());
         true;
       }
     );
};

let tupleOfLexing = ({Lexing.pos_lnum, pos_cnum, pos_bol}) => (
  pos_lnum - 1,
  pos_cnum - pos_bol,
);

/** Check if pos is within the location, but be fuzzy about when the location ends.
If it's within 5 lines, go with it.
 */
let locationContainsFuzzy = ({Location.loc_start, loc_end}, (l, c)) =>
  tupleOfLexing(loc_start) <= (l, c)
  && tupleOfLexing(loc_end) >= (l - 5, c);

/*
 * Quotes filename when not quoted
 * Example:
 * myFile.exe -> 'myFile.exe'
 * 'myFile.exe' -> 'myFile.exe'
 */
let maybeQuoteFilename = filename => {
  let len = String.length(filename);
  if (len < 1) {
    "";
  } else {
    let firstChar = filename.[0];
    let lastChar = filename.[len - 1];
    switch (firstChar, lastChar) {
    | ('\'', '\'')
    | ('"', '"') => filename
    | _ => Filename.quote(filename)
    };
  };
};

let filterMap = f => {
  let rec aux = accu =>
    fun
    | [] => List.rev(accu)
    | [x, ...l] =>
      switch (f(x)) {
      | None => aux(accu, l)
      | Some(v) => aux([v, ...accu], l)
      };

  aux([]);
};

let filterMapIndex = f => {
  let rec aux = (accu, i) =>
    fun
    | [] => List.rev(accu)
    | [x, ...l] =>
      switch (f(i, x)) {
      | None => aux(accu, i, l)
      | Some(v) => aux([v, ...accu], i + 1, l)
      };

  aux([], 0);
};
