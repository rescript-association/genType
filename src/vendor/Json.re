/** # Json parser
 *
 * Works with bucklescript and bsb-native
 *
 * ## Basics
 *
 * ```
 * open Json.Infix; /* for the nice infix operators */
 * let raw = {|{"hello": "folks"}|};
 * let who = Json.parse(raw) |> Json.get("hello") |?> Json.string;
 * Js.log(who);
 * ```
 *
 * ## Parse & stringify
 *
 * @doc parse, stringify
 *
 * ## Accessing descendents
 *
 * @doc get, nth, getPath
 *
 * ## Coercing to types
 *
 * @doc string, number, array, obj, bool, null
 *
 * ## The JSON type
 *
 * @doc t
 *
 * ## Infix operators for easier working
 *
 * @doc Infix
 */;

type t =
  | String(string)
  | Number(float)
  | Array(list(t))
  | Object(list((string, t)))
  | True
  | False
  | Null;

let string_of_number = (f) => {
  let s = string_of_float(f);
  if (s.[String.length(s) - 1] == '.') {
    String.sub(s, 0, String.length(s) - 1)
  } else {
    s
  }
};

/**
 * This module is provided for easier working with optional values.
 */
module Infix = {
  /** The "force unwrap" operator
   *
   * If you're sure there's a value, you can force it.
   * ```
   * open Json.Infix;
   * let x: int = Some(10) |! "Expected this to be present";
   * Js.log(x);
   * ```
   *
   * But you gotta be sure, otherwise it will throw.
   * ```reason;raises
   * open Json.Infix;
   * let x: int = None |! "This will throw";
   * ```
   */
  let (|!) = (o, d) =>
    switch o {
    | None => failwith(d)
    | Some(v) => v
    };
  /** The "upwrap with default" operator
   * ```
   * open Json.Infix;
   * let x: int = Some(10) |? 4;
   * let y: int = None |? 5;
   * Js.log2(x, y);
   * ```
   */
  let (|?) = (o, d) =>
    switch o {
    | None => d
    | Some(v) => v
    };
  /** The "transform contents into new optional" operator
   * ```
   * open Json.Infix;
   * let maybeInc = x => x > 5 ? Some(x + 1) : None;
   * let x: option(int) = Some(14) |?> maybeInc;
   * let y: option(int) = None |?> maybeInc;
   * ```
   */
  let (|?>) = (o, fn) =>
    switch o {
    | None => None
    | Some(v) => fn(v)
    };
  /** The "transform contents into new value & then re-wrap" operator
   * ```
   * open Json.Infix;
   * let inc = x => x + 1;
   * let x: option(int) = Some(7) |?>> inc;
   * let y: option(int) = None |?>> inc;
   * Js.log2(x, y);
   * ```
   */
  let (|?>>) = (o, fn) =>
    switch o {
    | None => None
    | Some(v) => Some(fn(v))
    };
  /** "handle the value if present, otherwise here's the default"
   *
   * It's called fold because that's what people call it :?. It's the same as "transform contents to new value" + "unwrap with default".
   *
   * ```
   * open Json.Infix;
   * let inc = x => x + 1;
   * let x: int = fold(Some(4), 10, inc);
   * let y: int = fold(None, 2, inc);
   * Js.log2(x, y);
   * ```
   */
  let fold = (o, d, f) =>
    switch o {
    | None => d
    | Some(v) => f(v)
    };
};

let escape = (text) => {
  let ln = String.length(text);
  let buf = Buffer.create(ln);
  let rec loop = (i) =>
    if (i < ln) {
      switch text.[i] {
      | '\012' => Buffer.add_string(buf, "\\f")
      | '\\' => Buffer.add_string(buf, "\\\\")
      | '"' => Buffer.add_string(buf, "\\\"")
      | '\n' => Buffer.add_string(buf, "\\n")
      | '\b' => Buffer.add_string(buf, "\\b")
      | '\r' => Buffer.add_string(buf, "\\r")
      | '\t' => Buffer.add_string(buf, "\\t")
      | c => Buffer.add_char(buf, c)
      };
      loop(i + 1)
    };
  loop(0);
  Buffer.contents(buf)
};

/** ```
 * let text = {|{"hello": "folks", "aa": [2, 3, "four"]}|};
 * let result = Json.stringify(Json.parse(text));
 * Js.log(result);
 * assert(text == result);
 * ```
 */
let rec stringify = (t) =>
  switch t {
  | String(value) => "\"" ++ escape(value) ++ "\""
  | Number(num) => string_of_number(num)
  | Array(items) => "[" ++ String.concat(", ", List.map(stringify, items)) ++ "]"
  | Object(items) =>
    "{"
    ++ String.concat(
         ", ",
         List.map(((k, v)) => "\"" ++ String.escaped(k) ++ "\": " ++ stringify(v), items)
       )
    ++ "}"
  | True => "true"
  | False => "false"
  | Null => "null"
  };

let white = n => {
  let buffer = Buffer.create(n);
  for (i in 0 to n - 1) {
    Buffer.add_char(buffer, ' ')
  };
  Buffer.contents(buffer)
};

let rec stringifyPretty = (~indent=0, t) =>
  switch t {
  | String(value) => "\"" ++ escape(value) ++ "\""
  | Number(num) => string_of_number(num)
  | Array([]) => "[]"
  | Array([String(_) as contents]) => "[" ++ stringifyPretty(contents) ++ "]"
  | Array(items) => "[\n" ++ white(indent) ++ String.concat(",\n" ++ white(indent), List.map(stringifyPretty(~indent=indent + 2), items)) ++ "\n" ++ white(indent - 2) ++ "]"
  | Object([]) => "{}"
  | Object(items) =>
    "{\n" ++ white(indent)
    ++ String.concat(
         ",\n" ++ white(indent),
         List.map(((k, v)) => "\"" ++ String.escaped(k) ++ "\": " ++ stringifyPretty(~indent=indent + 2, v), items)
       )
    ++ "\n" ++ white(indent - 2) ++ "}"
  | True => "true"
  | False => "false"
  | Null => "null"
  };


let unwrap = (message, t) =>
  switch t {
  | Some(v) => v
  | None => failwith(message)
  };

[@nodoc]
module Parser = {
  let split_by = (~keep_empty=false, is_delim, str) => {
    let len = String.length(str);
    let rec loop = (acc, last_pos, pos) =>
      if (pos == (-1)) {
        if (last_pos == 0 && ! keep_empty) {
          acc
        } else {
          [String.sub(str, 0, last_pos), ...acc]
        }
      } else if (is_delim(str.[pos])) {
        let new_len = last_pos - pos - 1;
        if (new_len != 0 || keep_empty) {
          let v = String.sub(str, pos + 1, new_len);
          loop([v, ...acc], pos, pos - 1)
        } else {
          loop(acc, pos, pos - 1)
        }
      } else {
        loop(acc, last_pos, pos - 1)
      };
    loop([], len, len - 1)
  };
  let fail = (text, pos, message) => {
    let pre = String.sub(text, 0, pos);
    let lines = split_by((c) => c == '\n', pre);
    let count = List.length(lines);
    let last = count > 0 ? List.nth(lines, count - 1) : "";
    let col = String.length(last) + 1;
    let line = List.length(lines);
    let string = Printf.sprintf("Error \"%s\" at %d:%d -> %s\n", message, line, col, last);
    failwith(string)
  };
  let rec skipToNewline = (text, pos) =>
    if (pos >= String.length(text)) {
      pos
    } else if (text.[pos] == '\n') {
      pos + 1
    } else {
      skipToNewline(text, pos + 1)
    };
  let stringTail = (text) => {
    let len = String.length(text);
    if (len > 1) {
      String.sub(text, 1, len - 1)
    } else {
      ""
    }
  };
  let rec skipToCloseMultilineComment = (text, pos) =>
    if (pos + 1 >= String.length(text)) {
      failwith("Unterminated comment")
    } else if (text.[pos] == '*' && text.[pos + 1] == '/') {
      pos + 2
    } else {
      skipToCloseMultilineComment(text, pos + 1)
    };
  let rec skipWhite = (text, pos) =>
    if (pos < String.length(text)
        && (text.[pos] == ' ' || text.[pos] == '\t' || text.[pos] == '\n' || text.[pos] == '\r')) {
      skipWhite(text, pos + 1)
    } else {
      pos
    };

  /* from https://stackoverflow.com/a/42431362 */
  let utf8encode = (s) => {
    let prefs = [|0, 192, 224|];
    let s1 = (n) => String.make(1, Char.chr(n));
    let rec ienc = (k, sofar, resid) => {
      let bct =
        if (k == 0) {
          7;
        } else {
          6 - k;
        };
      if (resid < 1 lsl bct) {
        s1(prefs[k] + resid) ++ sofar;
      } else {
        ienc(k + 1, s1(128 + resid mod 64) ++ sofar, resid / 64);
      };
    };
    ienc(0, "", int_of_string("0x" ++ s));
  };

  let parseString = (text, pos) => {
    /* let i = ref(pos); */
    let buffer = Buffer.create(String.length(text));
    let ln = String.length(text);
    let rec loop = (i) =>
      i >= ln ?
        fail(text, i, "Unterminated string") :
        (
          switch text.[i] {
          | '"' => i + 1
          | '\\' =>
            i + 1 >= ln ?
              fail(text, i, "Unterminated string") :
              (
                switch text.[i + 1] {
                | '/' =>
                  Buffer.add_char(buffer, '/');
                  loop(i + 2)
                | 'f' =>
                  Buffer.add_char(buffer, '\012');
                  loop(i + 2)
                | 'u' when i + 6 < ln =>
                  Buffer.add_string(buffer, utf8encode(String.sub(text, i + 2, 4)));
                  loop(i + 7)
                | _ =>
                  Buffer.add_string(buffer, Scanf.unescaped(String.sub(text, i, 2)));
                  loop(i + 2)
                }
              )
          | c =>
            Buffer.add_char(buffer, c);
            loop(i + 1)
          }
        );
    let final = loop(pos);
    (Buffer.contents(buffer), final)
  };
  let parseDigits = (text, pos) => {
    let len = String.length(text);
    let rec loop = (i) =>
      if (i >= len) {
        i
      } else {
        switch text.[i] {
        | '0'..'9' => loop(i + 1)
        | _ => i
        }
      };
    loop(pos + 1)
  };
  let parseWithDecimal = (text, pos) => {
    let pos = parseDigits(text, pos);
    if (pos < String.length(text) && text.[pos] == '.') {
      let pos = parseDigits(text, pos + 1);
      pos
    } else {
      pos
    }
  };
  let parseNumber = (text, pos) => {
    let pos = parseWithDecimal(text, pos);
    let ln = String.length(text);
    if (pos < ln - 1 && (text.[pos] == 'E' || text.[pos] == 'e')) {
      let pos =
        switch text.[pos + 1] {
        | '-'
        | '+' => pos + 2
        | _ => pos + 1
        };
      parseDigits(text, pos)
    } else {
      pos
    }
  };
  let parseNegativeNumber = (text, pos) => {
    let final =
      if (text.[pos] == '-') {
        parseNumber(text, pos + 1)
      } else {
        parseNumber(text, pos)
      };
    (Number(float_of_string(String.sub(text, pos, final - pos))), final)
  };
  let expect = (char, text, pos, message) =>
    if (text.[pos] != char) {
      fail(text, pos, "Expected: " ++ message)
    } else {
      pos + 1
    };
  let parseComment: 'a .(string, int, (string, int) => 'a) => 'a =
    (text, pos, next) =>
      if (text.[pos] != '/') {
        if (text.[pos] == '*') {
          next(text, skipToCloseMultilineComment(text, pos + 1))
        } else {
          failwith("Invalid syntax")
        }
      } else {
        next(text, skipToNewline(text, pos + 1))
      };
  let maybeSkipComment = (text, pos) =>
    if (pos < String.length(text) && text.[pos] == '/') {
      if (pos + 1 < String.length(text) && text.[pos + 1] == '/') {
        skipToNewline(text, pos + 1)
      } else if (pos + 1 < String.length(text) && text.[pos + 1] == '*') {
        skipToCloseMultilineComment(text, pos + 1)
      } else {
        fail(text, pos, "Invalid synatx")
      }
    } else {
      pos
    };
  let rec skip = (text, pos) =>
    if (pos == String.length(text)) {
      pos
    } else {
      let n = skipWhite(text, pos) |> maybeSkipComment(text);
      if (n > pos) {
        skip(text, n)
      } else {
        n
      }
    };
  let rec parse = (text, pos) =>
    if (pos >= String.length(text)) {
      fail(text, pos, "Reached end of file without being done parsing")
    } else {
      switch text.[pos] {
      | '/' => parseComment(text, pos + 1, parse)
      | '[' => parseArray(text, pos + 1)
      | '{' => parseObject(text, pos + 1)
      | 'n' =>
        if (String.sub(text, pos, 4) == "null") {
          (Null, pos + 4)
        } else {
          fail(text, pos, "unexpected character")
        }
      | 't' =>
        if (String.sub(text, pos, 4) == "true") {
          (True, pos + 4)
        } else {
          fail(text, pos, "unexpected character")
        }
      | 'f' =>
        if (String.sub(text, pos, 5) == "false") {
          (False, pos + 5)
        } else {
          fail(text, pos, "unexpected character")
        }
      | '\n'
      | '\t'
      | ' '
      | '\r' => parse(text, skipWhite(text, pos))
      | '"' =>
        let (s, pos) = parseString(text, pos + 1);
        (String(s), pos)
      | '-'
      | '0'..'9' => parseNegativeNumber(text, pos)
      | _ => fail(text, pos, "unexpected character")
      }
    }
  and parseArrayValue = (text, pos) => {
    let pos = skip(text, pos);
    let (value, pos) = parse(text, pos);
    let pos = skip(text, pos);
    switch text.[pos] {
    | ',' =>
      let pos = skip(text, pos + 1);
      if (text.[pos] == ']') {
        ([value], pos + 1)
      } else {
        let (rest, pos) = parseArrayValue(text, pos);
        ([value, ...rest], pos)
      }
    | ']' => ([value], pos + 1)
    | _ => fail(text, pos, "unexpected character")
    }
  }
  and parseArray = (text, pos) => {
    let pos = skip(text, pos);
    switch text.[pos] {
    | ']' => (Array([]), pos + 1)
    | _ =>
      let (items, pos) = parseArrayValue(text, pos);
      (Array(items), pos)
    }
  }
  and parseObjectValue = (text, pos) => {
    let pos = skip(text, pos);
    if (text.[pos] != '"') {
      fail(text, pos, "Expected string")
    } else {
      let (key, pos) = parseString(text, pos + 1);
      let pos = skip(text, pos);
      let pos = expect(':', text, pos, "Colon");
      let (value, pos) = parse(text, pos);
      let pos = skip(text, pos);
      switch text.[pos] {
      | ',' =>
        let pos = skip(text, pos + 1);
        if (text.[pos] == '}') {
          ([(key, value)], pos + 1)
        } else {
          let (rest, pos) = parseObjectValue(text, pos);
          ([(key, value), ...rest], pos)
        }
      | '}' => ([(key, value)], pos + 1)
      | _ =>
        let (rest, pos) = parseObjectValue(text, pos);
        ([(key, value), ...rest], pos)
      }
    }
  }
  and parseObject = (text, pos) => {
    let pos = skip(text, pos);
    if (text.[pos] == '}') {
      (Object([]), pos + 1)
    } else {
      let (pairs, pos) = parseObjectValue(text, pos);
      (Object(pairs), pos)
    }
  };
};

/** Turns some text into a json object. throws on failure */
let parse = (text) => {
  let (item, pos) = Parser.parse(text, 0);
  let pos = Parser.skip(text, pos);
  if (pos < String.length(text)) {
    failwith(
      "Extra data after parse finished: " ++ String.sub(text, pos, String.length(text) - pos)
    )
  } else {
    item
  }
};

/* Accessor helpers */
let bind = (v, fn) =>
  switch v {
  | None => None
  | Some(v) => fn(v)
  };

/** If `t` is an object, get the value associated with the given string key */
let get = (key, t) =>
  switch t {
  | Object(items) =>
    try (Some(List.assoc(key, items))) {
    | Not_found => None
    }
  | _ => None
  };

/** If `t` is an array, get the value associated with the given index */
let nth = (n, t) =>
  switch t {
  | Array(items) =>
    if (n < List.length(items)) {
      Some(List.nth(items, n))
    } else {
      None
    }
  | _ => None
  };

let string = (t) =>
  switch t {
  | String(s) => Some(s)
  | _ => None
  };

let number = (t) =>
  switch t {
  | Number(s) => Some(s)
  | _ => None
  };

let array = (t) =>
  switch t {
  | Array(s) => Some(s)
  | _ => None
  };

let obj = (t) =>
  switch t {
  | Object(s) => Some(s)
  | _ => None
  };

let bool = (t) =>
  switch t {
  | True => Some(true)
  | False => Some(false)
  | _ => None
  };

let null = (t) =>
  switch t {
  | Null => Some()
  | _ => None
  };

let rec parsePath = (keyList, t) =>
  switch keyList {
  | [] => Some(t)
  | [head, ...rest] =>
    switch (get(head, t)) {
    | None => None
    | Some(value) => parsePath(rest, value)
    }
  };

/** Get a deeply nested value from an object `t`.
 * ```
 * open Json.Infix;
 * let json = Json.parse({|{"a": {"b": {"c": 2}}}|});
 * let num = Json.getPath("a.b.c", json) |?> Json.number;
 * assert(num == Some(2.))
 * ```
 */
let getPath = (path, t) => {
  let keys = Parser.split_by((c) => c == '.', path);
  parsePath(keys, t)
};