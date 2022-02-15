/* @flow strict */

// flowlint untyped-import:off
import * as ReactDOM from "react-dom";
import * as React from "react";

import * as SomeFlowTypes from "../SomeFlowTypes";

import * as Variants from "../Variants.gen";

import Hooks from "../Hooks.gen";

import {
  printVariantWithPayload,
  printManyPayloads,
  testManyPayloads,
  testWithPayload,
  testVariantWithPayloads,
  printVariantWithPayloads,
} from "../VariantsWithPayload.gen";

const consoleLog = console.log;

import * as WrapJsValue from "./ImportJsValue.gen";

consoleLog("interopRoot.js roundedNumber:", WrapJsValue.roundedNumber);
consoleLog("interopRoot.js areaValue:", WrapJsValue.areaValue);

consoleLog("anInterestingFlowType ", SomeFlowTypes.c);

consoleLog("Variants: swap(sunday) =", Variants.swap("sunday"));
consoleLog("Variants: fortytwoOK is", Variants.fortytwoOK);
consoleLog("Variants: fortytwoBAD is", Variants.fortytwoBAD);
consoleLog(
  "Variants: testConvert3to2('module') =",
  Variants.testConvert2to3("module")
);
consoleLog("Variants: testConvert3to2('42') =", Variants.testConvert2to3("42"));

printVariantWithPayload("a");
printVariantWithPayload("bRenamed");
printVariantWithPayload(true);
printVariantWithPayload(20);
printVariantWithPayload(0.5);
printVariantWithPayload(testWithPayload({ NAME: "c", VAL: { x: 15 } }));

printManyPayloads({ NAME: "oneRenamed", VAL: 34 });
printManyPayloads({ NAME: 2, VAL: ["hello", "world"] });
printManyPayloads(testManyPayloads({ NAME: "three", VAL: { x: 15 } }));

printVariantWithPayloads(testVariantWithPayloads("ARenamed"));
printVariantWithPayloads(testVariantWithPayloads({ tag: "B", value: 4 }));
printVariantWithPayloads(testVariantWithPayloads({ tag: "C", value: [1, 2] }));
printVariantWithPayloads(testVariantWithPayloads({ tag: "D", value: [1, 2] }));
printVariantWithPayloads(
  testVariantWithPayloads({ tag: "E", value: [1, "hello", 2] })
);

const App = () => (
  <div>
    <Hooks vehicle={{ name: "Car" }} />
  </div>
);
App.displayName = "ExampleInteropRoot";

// $FlowExpectedError[untyped-import]: Reason checked type sufficiently
ReactDOM.render(React.createElement(App), document.getElementById("index"));
