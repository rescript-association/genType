/** Immutable arrays are covariant. */
type t(+'a);

/** Redefine the [_] syntax, and disable the assignment [_] = _. */
module Array: {let get: (t('a), int) => option('a);};

/** Converting from/to normal arrays involves making a copy. */
let fromArray: array('a) => t('a);

[@dead "toArray"] let toArray: t('a) => array('a);

[@dead "length"] /** Subset of the Belt.Array oprerations that do not mutate the array. */

let length: t('a) => int;

[@dead "size"] let size: t('a) => int;

[@dead "get"] let get: (t('a), int) => option('a);

[@dead "getExn"] let getExn: (t('a), int) => 'a;

[@dead "getUnsafe"] let getUnsafe: (t('a), int) => 'a;

[@dead "getUndefined"] let getUndefined: (t('a), int) => Js.undefined('a);

[@dead "shuffle"] let shuffle: t('a) => t('a);

[@dead "reverse"] let reverse: t('a) => t('a);

[@dead "makeUninitialized"] let makeUninitialized: int => t(Js.undefined('a));

[@dead "makeUninitializedUnsafe"] let makeUninitializedUnsafe: int => t('a);

[@dead "make"] let make: (int, 'a) => t('a);

[@dead "range"] let range: (int, int) => t(int);

[@dead "rangeBy"] let rangeBy: (int, int, ~step: int) => t(int);

[@dead "makeByU"] let makeByU: (int, (. int) => 'a) => t('a);
[@dead "makeBy"] let makeBy: (int, int => 'a) => t('a);

[@dead "makeByAndShuffleU"] let makeByAndShuffleU: (int, (. int) => 'a) => t('a);
[@dead "makeByAndShuffle"] let makeByAndShuffle: (int, int => 'a) => t('a);

[@dead "zip"] let zip: (t('a), t('b)) => t(('a, 'b));

[@dead "zipByU"] let zipByU: (t('a), t('b), (. 'a, 'b) => 'c) => t('c);
[@dead "zipBy"] let zipBy: (t('a), t('b), ('a, 'b) => 'c) => t('c);

[@dead "unzip"] let unzip: t(('a, 'a)) => (t('a), t('a));

[@dead "concat"] let concat: (t('a), t('a)) => t('a);

[@dead "concatMany"] let concatMany: t(t('a)) => t('a);

[@dead "slice"] let slice: (t('a), ~offset: int, ~len: int) => t('a);

[@dead "sliceToEnd"] let sliceToEnd: (t('a), int) => t('a);

[@dead "copy"] let copy: t('a) => t('a);

[@dead "forEachU"] let forEachU: (t('a), (. 'a) => unit) => unit;
[@dead "forEach"] let forEach: (t('a), 'a => unit) => unit;

[@dead "mapU"] let mapU: (t('a), (. 'a) => 'b) => t('b);
[@dead "map"] let map: (t('a), 'a => 'b) => t('b);

[@dead "keepWithIndexU"] let keepWithIndexU: (t('a), (. 'a, int) => bool) => t('a);
[@dead "keepWithIndex"] let keepWithIndex: (t('a), ('a, int) => bool) => t('a);

[@dead "keepMapU"] let keepMapU: (t('a), (. 'a) => option('b)) => t('b);
[@dead "keepMap"] let keepMap: (t('a), 'a => option('b)) => t('b);

[@dead "forEachWithIndexU"] let forEachWithIndexU: (t('a), (. int, 'a) => unit) => unit;
[@dead "forEachWithIndex"] let forEachWithIndex: (t('a), (int, 'a) => unit) => unit;

[@dead "mapWithIndexU"] let mapWithIndexU: (t('a), (. int, 'a) => 'b) => t('b);
[@dead "mapWithIndex"] let mapWithIndex: (t('a), (int, 'a) => 'b) => t('b);

[@dead "partitionU"] let partitionU: (t('a), (. 'a) => bool) => (t('a), t('a));
[@dead "partition"] let partition: (t('a), 'a => bool) => (t('a), t('a));

[@dead "reduceU"] let reduceU: (t('a), 'b, (. 'b, 'a) => 'b) => 'b;
[@dead "reduce"] let reduce: (t('a), 'b, ('b, 'a) => 'b) => 'b;

[@dead "reduceReverseU"] let reduceReverseU: (t('a), 'b, (. 'b, 'a) => 'b) => 'b;
[@dead "reduceReverse"] let reduceReverse: (t('a), 'b, ('b, 'a) => 'b) => 'b;

[@dead "reduceReverse2U"] let reduceReverse2U: (t('a), t('b), 'c, (. 'c, 'a, 'b) => 'c) => 'c;
[@dead "reduceReverse2"] let reduceReverse2: (t('a), t('b), 'c, ('c, 'a, 'b) => 'c) => 'c;

[@dead "someU"] let someU: (t('a), (. 'a) => bool) => bool;
[@dead "some"] let some: (t('a), 'a => bool) => bool;

[@dead "everyU"] let everyU: (t('a), (. 'a) => bool) => bool;
[@dead "every"] let every: (t('a), 'a => bool) => bool;

[@dead "every2U"] let every2U: (t('a), t('b), (. 'a, 'b) => bool) => bool;
[@dead "every2"] let every2: (t('a), t('b), ('a, 'b) => bool) => bool;

[@dead "some2U"] let some2U: (t('a), t('b), (. 'a, 'b) => bool) => bool;
[@dead "some2"] let some2: (t('a), t('b), ('a, 'b) => bool) => bool;

[@dead "cmpU"] let cmpU: (t('a), t('a), (. 'a, 'a) => int) => int;
[@dead "cmp"] let cmp: (t('a), t('a), ('a, 'a) => int) => int;

[@dead "eqU"] let eqU: (t('a), t('a), (. 'a, 'a) => bool) => bool;
[@dead "eq"] let eq: (t('a), t('a), ('a, 'a) => bool) => bool;