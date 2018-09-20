let component = ReasonReact.statelessComponent("App");

[@genFlow]
let make = (_children) => {
    ...component,
    render: _self => {
        <div>(ReasonReact.string("Test"))</div>
    }
};