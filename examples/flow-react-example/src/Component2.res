/* State declaration */
type state = {
  count: int,
  show: bool,
  someRef: ref<int>,
}

/* Action declaration */
type action =
  | Click
  | Toggle

@genType @react.component
let make = (~greeting) => {
  let message = "You've clicked this " ++ ("xx" ++ " times(s)")
  <div>
    <button> {React.string(message)} </button>
    <button> {React.string("Toggle greeting")} </button>
    {React.string(greeting)}
  </div>
}

@genType
type variant =
  | A
  | B(int, int)
  | C(option<int>)

/* No name clash with Block */
@genType
type block = Block

@genType
let getBlock = x =>
  switch x {
  | Block => 34
  }

