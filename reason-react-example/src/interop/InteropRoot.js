/* @flow strict */

const ReactDOM = require("react-dom");
const React = require("react");

const GreetingRe = require("./Greeting.re");

// Import a ReasonReact component!
const PageReason = GreetingRe.component;

const helloWorldList = GreetingRe.cons({
  x: "Hello",
  l: GreetingRe.cons2({ x: "World", l: GreetingRe.empty })
});

const helloWorld = GreetingRe.concat("++", helloWorldList);

const someNumber: number = GreetingRe.testDefaultArgs({ y: 10 });

const App = () => (
  <div>
    <PageReason message={helloWorld} someNumber={someNumber} />
  </div>
);
App.displayName = "ExampleInteropRoot";

// $FlowFixMe
ReactDOM.render(React.createElement(App), document.getElementById("index"));
