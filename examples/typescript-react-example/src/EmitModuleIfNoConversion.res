export type t =
  | A
  | B({name: string})

// foo requires converion: don't emit module X
module X = {
  export foo = (t: t) =>
    switch t {
    | A => Js.log("A")
    | B({name}) => Js.log("B" ++ name)
    }

  export x = 42
}

// No field requires converion: emit module Y
module Y = {
  export x = ""
}
