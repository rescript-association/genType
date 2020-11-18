/**
 * This combines a filter and a map.
 * You provide a function that turns an element into an optional of another element,
 * and you get a list of all of the present results.
 */
let optMap: ('a => option('b), list('a)) => list('b) =
  (fn, items) =>
    List.fold_left(
      (result, item) =>
        switch (fn(item)) {
        | None => result
        | Some(res) => [res, ...result]
        },
      [],
      items,
    );

let (|!) = (o, d) =>
  switch (o) {
  | None => failwith(d)
  | Some(v) => v
  };
let (|?) = (o, d) =>
  switch (o) {
  | None => d
  | Some(v) => v
  };
let (|??) = (o, d) =>
  switch (o) {
  | None => d
  | Some(v) => Some(v)
  };
let (|?>) = (o, fn) =>
  switch (o) {
  | None => None
  | Some(v) => fn(v)
  };
let (|?>>) = (o, fn) =>
  switch (o) {
  | None => None
  | Some(v) => Some(fn(v))
  };
let fold = (o, d, f) =>
  switch (o) {
  | None => d
  | Some(v) => f(v)
  };

let (|?<) = (o, fn) =>
  switch (o) {
  | None => ()
  | Some(v) => fn(v)
  };

let fileConcat = (a, b) =>
  if (b != ""
      && b.[0] == '.'
      && String.length(b) >= 2
      && b.[1] == Filename.dir_sep.[0]) {
    Filename.concat(a, String.sub(b, 2, String.length(b) - 2));
  } else {
    Filename.concat(a, b);
  };

let logIfAbsent = (message, x) =>
  switch (x) {
  | None =>
    Log.log(message);
    None;
  | _ => x
  };

let isFullPath = b =>
  b.[0] == '/' || Sys.win32 && String.length(b) > 1 && b.[1] == ':';

let maybeConcat = (a, b) =>
  if (b != "" && isFullPath(b)) {
    b;
  } else {
    fileConcat(a, b);
  };

let (/+) = fileConcat;
