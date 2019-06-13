/* State declaration */
type state = {
  count: int,
  show: bool,
  someRef: ref(int),
};

/* Action declaration */
type action =
  | Click
  | Toggle;

/* Component template declaration.
   Needs to be **after** state and action declarations! */
let component = ReasonReact.reducerComponent("Example");

/* greeting and children are props. `children` isn't used, therefore ignored.
   We ignore it by prepending it with an underscore */
[@genType]
let make = (~greeting, _children) => {
  /* spread the other default fields of component here and override a few */
  ...component,
  initialState: () => {count: 0, show: true, someRef: ref(0)},
  /* State transitions */
  reducer: (action, state) =>
    switch (action) {
    | Click => ReasonReact.Update({...state, count: state.count + 1})
    | Toggle => ReasonReact.Update({...state, show: !state.show})
    },
  render: self => {
    let message =
      "You've clicked this " ++ string_of_int(self.state.count) ++ " times(s)";
    <div>
      <button onClick={_event => self.send(Click)}>
        {ReasonReact.string(message)}
      </button>
      <button onClick={_event => self.send(Toggle)}>
        {ReasonReact.string("Toggle greeting")}
      </button>
      {self.state.show ? ReasonReact.string(greeting) : ReasonReact.null}
    </div>;
  },
};

[@genType]
type variant =
  | A
  | B(int, int)
  | C(option(int));

/* No name clash with Block */
[@genType]
type block =
  | Block;

[@genType]
let getBlock = x =>
  switch (x) {
  | Block => 34
  };