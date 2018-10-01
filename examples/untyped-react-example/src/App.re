let component = ReasonReact.statelessComponent("App");

[@genType]
let make = (_children) => {
    ...component,
    render: _self => {
        <div>(ReasonReact.string("Test"))</div>
    }
};