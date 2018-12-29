/* @flow strict */

import * as ReactDOM from "react-dom";
import * as React from "react";

import * as GreetingRe from "./Greeting.gen";

// Import a ReasonReact component!
import Greeting from "./Greeting.gen";

import {InnerComponent} from "../components/ManyComponents.gen";

import * as SomeFlowTypes from "../SomeFlowTypes";

import * as Enums  from "../Enums.gen";

import { printEnumValue, testWithPayload } from "../EnumsWithPayload.gen";

const consoleLog = console.log;

const helloWorldList = GreetingRe.cons({
  x: "Hello",
  l: GreetingRe.cons2({ x: "World", l: GreetingRe.empty })
});

const helloWorld = GreetingRe.concat("++", helloWorldList);

const someNumber: number = GreetingRe.testDefaultArgs({ y: 10 });

import * as WrapJsValue from "./ImportJsValue.gen";

consoleLog("interopRoot.js roundedNumber:", WrapJsValue.roundedNumber);
consoleLog("interopRoot.js areaValue:", WrapJsValue.areaValue);

// type error: can't call that directly
//  const callAreaDirectly = WrapJsValue.area({x:3,y:4});

const callMyAreaDirectly = WrapJsValue.myArea({ x: 3, y: 4 });

consoleLog("interopRoot.js callMyAreaDirectly:", callMyAreaDirectly);

consoleLog("anInterestingFlowType ", SomeFlowTypes.c);

consoleLog("Enums: swap(sunday) =", Enums.swap("sunday"));
consoleLog("Enums: fortytwoOK is", Enums.fortytwoOK);
consoleLog("Enums: fortytwoBAD is", Enums.fortytwoBAD);
consoleLog(
  "Enums: testConvert3to2('module') =",
  Enums.testConvert2to3("module")
);
consoleLog("Enums: testConvert3to2('42') =", Enums.testConvert2to3("42"));

printEnumValue("a");
printEnumValue("bRenamed");
printEnumValue(true);
printEnumValue(20);
printEnumValue(.5);
printEnumValue(testWithPayload({ x: 15 }));

const App = () => (
  <div>
    <Greeting
      message={helloWorld}
      someNumber={someNumber}
      polymorphicProp={[1, 2, 3]}
    />
    <InnerComponent />
  </div>
);
App.displayName = "ExampleInteropRoot";

// $FlowExpectedError: Reason checked type sufficiently
ReactDOM.render(React.createElement(App), document.getElementById("index"));
