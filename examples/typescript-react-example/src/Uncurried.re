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

[@genType]
let callback = cb => cb() |> string_of_int;

type auth = {login: unit => string};

[@genType]
let callback2 = auth => auth.login();

[@genType]
let sumCurried = n => {
  Js.log2("sum 1st arg", n);
  m => {
    Js.log4("sum 2nd arg", m, "result", n + m);
  };
};