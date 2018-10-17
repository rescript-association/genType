/* @flow strict */

const ReactDOM = require("react-dom");
const React = require("react");

const GreetingRe = require("./Greeting.re");

// Import a ReasonReact component!
const PageReason = require("./Greeting.re").default;

const helloWorldList = GreetingRe.cons({
  x: "Hello",
  l: GreetingRe.cons2({ x: "World", l: GreetingRe.empty })
});

const helloWorld = GreetingRe.concat("++", helloWorldList);

const someNumber: number = GreetingRe.testDefaultArgs({ y: 10 });

const WrapJsValue = require("./WrapJsValue.re");
console.log("interopRoot.js roundedNumber:", WrapJsValue.roundedNumber);
console.log("interopRoot.js areaValue:", WrapJsValue.areaValue);

console.log("anInterestingFlowType ", require("../basics/SomeFlowTypes").c);

const Enums = require("../basics/Enums.re");

console.log("swap(sunday) =", Enums.swap("sunday"));
console.log("fortytwoOK is", Enums.fortytwoOK);
console.log("fortytwoBAD is", Enums.fortytwoBAD);
console.log("testConvert3to2('module') =", Enums.testConvert2to3("module"));
console.log("testConvert3to2('42') =", Enums.testConvert2to3("42"));

const App = () => (
  <div>
    <PageReason
      message={helloWorld}
      someNumber={someNumber}
      polymorphicProp={[1, 2, 3]}
    />
  </div>
);
App.displayName = "ExampleInteropRoot";

// $FlowExpectedError: Reason checked type sufficiently
ReactDOM.render(React.createElement(App), document.getElementById("index"));
