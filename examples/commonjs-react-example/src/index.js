/* @flow strict */

const ReactDOM = require("react-dom");
const React = require("react");

const Greeting = require("./Greeting.gen").default;

const InnerComponent = require("./ManyComponents.gen").InnerComponent;

const consoleLog = console.log;

const App = () => (
  <div>
    <Greeting message={"Hello Worldd"} someNumber={42} />
    <InnerComponent />
  </div>
);
App.displayName = "ExampleInteropRoot";

// $FlowExpectedError: Reason checked type sufficiently
ReactDOM.render(React.createElement(App), document.getElementById("index"));
