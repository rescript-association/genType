export type u0 = (. unit) => string

export type u1 = (. int) => string

export type u2 = (. int, string) => string

export type u3 = (. int, string, int) => string

export uncurried0 = (. ()) => ""

export uncurried1 = (. x) => x |> string_of_int

export uncurried2 = (. x, y) => (x |> string_of_int) ++ y

export uncurried3 = (. x, y, z) => (x |> string_of_int) ++ (y ++ (z |> string_of_int))

export curried3 = (x, y, z) => (x |> string_of_int) ++ (y ++ (z |> string_of_int))

export callback = cb => cb() |> string_of_int

type auth = {login: unit => string}
type authU = {loginU: (. unit) => string}

export callback2 = auth => auth.login()

export callback2U = auth => auth.loginU(.)

export sumU = (. n, m) => Js.log4("sumU 2nd arg", m, "result", n + m)

export sumU2 = (. n, . m) => Js.log4("sumU2 2nd arg", m, "result", n + m)

export sumCurried = n => {
  Js.log2("sumCurried 1st arg", n)
  m => Js.log4("sumCurried 2nd arg", m, "result", n + m)
}

export sumLblCurried = (s: string, ~n) => {
  Js.log3(s, "sumLblCurried 1st arg", n)
  (~m) => Js.log4("sumLblCurried 2nd arg", m, "result", n + m)
}
