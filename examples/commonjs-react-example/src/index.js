/* @flow strict */

// flowlint untyped-import:off
const ReactDOM = require("react-dom");
const React = require("react");

const Hooks = require("./Hooks.gen").default;


const consoleLog = console.log;

const App = () => (
  <div>
    <Hooks vehicle={{ name: "Car" }}/>
  </div>
);
App.displayName = "ExampleInteropRoot";

// $FlowExpectedError: Reason checked type sufficiently
ReactDOM.render(React.createElement(App), document.getElementById("index"));
