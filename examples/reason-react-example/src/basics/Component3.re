let component = ReasonReact.statelessComponent("Component3");

/* example component without props */
[@genFlow]
let make = _children => {...component, render: _self => <div />};