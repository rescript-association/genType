/* @flow strict */

const ReactDOM = require("react-dom");
const React = require("react");

const GreetingRe = require("./Greeting.re");

// Import a ReasonReact component!
const PageReason = require("./Greeting.re").default;

const consoleLog = console.log;

const helloWorldList = GreetingRe.cons({
  x: "Hello",
  l: GreetingRe.cons2({ x: "World", l: GreetingRe.empty })
});

const helloWorld = GreetingRe.concat("++", helloWorldList);

const someNumber: number = GreetingRe.testDefaultArgs({ y: 10 });

const WrapJsValue = require("./WrapJsValue.re");

consoleLog("interopRoot.js roundedNumber:", WrapJsValue.roundedNumber);
consoleLog("interopRoot.js areaValue:", WrapJsValue.areaValue);

consoleLog("anInterestingFlowType ", require("../basics/SomeFlowTypes").c);

const Enums = require("../basics/Enums.re");

consoleLog("Enums: swap(sunday) =", Enums.swap("sunday"));
consoleLog("Enums: fortytwoOK is", Enums.fortytwoOK);
consoleLog("Enums: fortytwoBAD is", Enums.fortytwoBAD);
consoleLog("Enums: testConvert3to2('module') =", Enums.testConvert2to3("module"));
consoleLog("Enums: testConvert3to2('42') =", Enums.testConvert2to3("42"));

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
