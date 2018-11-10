let component = ReasonReact.statelessComponent("Component3");

/* example component without props */
[@genType]
let make = _children => {...component, render: _self => <div />};