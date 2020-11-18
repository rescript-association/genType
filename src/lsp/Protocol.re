open JsonShort;

let pos = (~line, ~character) =>
  o([("line", i(line)), ("character", i(character))]);

open Infix;

let rgetPosition = pos => {
  open RResult.InfixResult;
  let%try line = RJson.get("line", pos) |?> RJson.number;
  let%try character = RJson.get("character", pos) |?> RJson.number;
  Ok((int_of_float(line), int_of_float(character)));
};

let rgetRange = pos => {
  open RResult.InfixResult;
  let%try start = RJson.get("start", pos) |?> rgetPosition;
  let%try end_ = RJson.get("end", pos) |?> rgetPosition;
  Ok((start, end_));
};

let rPositionParams = params => {
  open RResult.InfixResult;
  let%try uri =
    RJson.get("textDocument", params) |?> RJson.get("uri") |?> RJson.string;
  let%try pos = RJson.get("position", params) |?> rgetPosition;
  Ok((uri, pos));
};

let posOfLexing = ({Lexing.pos_lnum, pos_cnum, pos_bol}) =>
  o([("line", i(pos_lnum - 1)), ("character", i(pos_cnum - pos_bol))]);

let contentKind = (useMarkdown, text) =>
  Json.Object([
    ("kind", Json.String(useMarkdown ? "markdown" : "text")),
    ("value", Json.String(text)),
  ]);

let rangeOfLoc = ({Location.loc_start, loc_end}) =>
  o([("start", posOfLexing(loc_start)), ("end", posOfLexing(loc_end))]);

let locationOfLoc =
    (~fname=?, {Location.loc_start: {Lexing.pos_fname}} as loc) =>
  o([
    ("range", rangeOfLoc(loc)),
    (
      "uri",
      s(
        switch (fname) {
        | Some(x) => x
        | None => Utils.toUri(pos_fname)
        },
      ),
    ),
  ]);

let rangeOfInts = (l0, c0, l1, c1) =>
  o([
    ("start", pos(~line=l0, ~character=c0)),
    ("end", pos(~line=l1, ~character=c1)),
  ]);

let locationContains = ({Location.loc_start, loc_end}, pos) =>
  Utils.tupleOfLexing(loc_start) <= pos
  && Utils.tupleOfLexing(loc_end) >= pos;

let symbolKind = (kind: SharedTypes.kinds) =>
  switch (kind) {
  | Module => 2
  | Enum => 10
  | Interface => 11
  | Function => 12
  | Variable => 13
  | Array => 18
  | Object => 19
  | Null => 21
  | EnumMember => 22
  | TypeParameter => 26
  };

/*
   returns true if a MarkupKind[] contains "markdown"
 */
let hasMarkdownCap = markupKind => {
  let%opt kinds = Json.array(markupKind) |?>> optMap(Json.string);
  Some(List.mem("markdown", kinds));
};
