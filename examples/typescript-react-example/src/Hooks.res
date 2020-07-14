type vehicle = {name: string}

@react.component
let make = (~vehicle) => {
  let (count, setCount) = React.useState(() => 0)

  <div>
    <p>
      {React.string(
        "Hooks example " ++ (vehicle.name ++ (" clicked " ++ (string_of_int(count) ++ " times"))),
      )}
    </p>
    <button onClick={_ => setCount(_ => count + 1)}> {React.string("Click me")} </button>
    <ImportHooks person={name: "Mary", age: 71} renderMe={x => React.string(x["randomString"])}>
      {React.string("child1")} {React.string("child2")}
    </ImportHooks>
    <ImportHookDefault
      person={name: "DefaultImport", age: 42} renderMe={x => React.string(x["randomString"])}>
      {React.string("child1")} {React.string("child2")}
    </ImportHookDefault>
  </div>
}

export default = make

@react.component
export anotherComponent = (~vehicle, ~callback: unit => unit) => {
  callback()
  <div> {React.string("Another Hook " ++ vehicle.name)} </div>
}

module Inner = {
  @react.component
  export make = (~vehicle) => <div> {React.string("Another Hook " ++ vehicle.name)} </div>

  @react.component
  export anotherComponent = (~vehicle) =>
    <div> {React.string("Another Hook " ++ vehicle.name)} </div>

  module Inner2 = {
    @react.component
    export make = (~vehicle) => <div> {React.string("Another Hook " ++ vehicle.name)} </div>

    @react.component
    export anotherComponent = (~vehicle) =>
      <div> {React.string("Another Hook " ++ vehicle.name)} </div>
  }
}

module NoProps = {
  @react.component
  export make = () => <div> ReasonReact.null </div>
}

type cb = (~_to: vehicle) => unit

export functionWithRenamedArgs = (~_to, ~_Type, ~cb: cb) => {
  cb(~_to)
  _to.name ++ _Type.name
}

@react.component
export componentWithRenamedArgs = (~_to, ~_Type, ~cb: cb) => {
  cb(~_to)
  React.string(_to.name ++ _Type.name)
}

@react.component
export makeWithRef = (~vehicle) => {
  let _ = 34
  ref =>
    switch ref->Js.Nullable.toOption {
    | Some(ref) => <button ref={ReactDOMRe.Ref.domRef(ref)}> {React.string(vehicle.name)} </button>
    | None => React.null
    }
}

export testForwardRef = React.forwardRef(makeWithRef)

type r = {x: string}

@react.component
export input = React.forwardRef((~r, (), ref) =>
  <div ref={Obj.magic(ref)}> {React.string(r.x)} </div>
)

export type callback<'input, 'output> = React.callback<'input, 'output>

export type testReactContext = React.Context.t<int>

export type testReactRef = React.Ref.t<int>

export type testDomRef = ReactDOMRe.domRef

@react.component
export polymorphicComponent = (~p as (x, _)) => React.string(x.name)

@react.component
export functionReturningReactElement = (~name) => React.string(name)

module RenderPropRequiresConversion = {
  @react.component
  export make = (~renderVehicle: {"vehicle": vehicle, "number": int} => React.element) => {
    let car = {name: "Car"}
    renderVehicle({"vehicle": car, "number": 42})
  }
}

@react.component
export aComponentWithChildren = (~vehicle, ~children) =>
  <div> {React.string("Another Hook " ++ vehicle.name)} <div> children </div> </div>
