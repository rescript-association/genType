[@genType]
type t = int;

[@genType]
let someIntList = [1, 2, 3];

[@genType]
let map = List.map;

[@genType]
type typeWithVars('x, 'y, 'z) =
  | A('x, 'y)
  | B('z);

[@genType]
type tree = {
  .
  "label": string,
  "left": option(tree),
  "right": option(tree),
};

[@genType]
let rec swap = (tree: tree) : tree => {
  "label": tree##label,
  "left": tree##right->(Belt.Option.map(swap)),
  "right": tree##left->(Belt.Option.map(swap)),
};

[@genType]
type selfRecursive = {self: selfRecursive};

[@genType]
type mutuallyRecursiveA = {b: mutuallyRecursiveB}
and mutuallyRecursiveB = {a: mutuallyRecursiveA};

[@genType]
let selfRecursiveConverter = ({self}) => self;

[@genType]
let mutuallyRecursiveConverter = ({b}) => b;