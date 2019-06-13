[@genType]
type u0 = (. unit) => string;

[@genType]
type u1 = (. int) => string;

[@genType]
type u2 = (. int, string) => string;

[@genType]
type u3 = (. int, string, int) => string;

[@genType]
let uncurried0 = (.) => "";

[@genType]
let uncurried1 = (. x) => x |> string_of_int;

[@genType]
let uncurried2 = (. x, y) => (x |> string_of_int) ++ y;

[@genType]
let uncurried3 =
  (. x, y, z) => (x |> string_of_int) ++ y ++ (z |> string_of_int);

[@genType]
let curried3 = (x, y, z) => (x |> string_of_int) ++ y ++ (z |> string_of_int);