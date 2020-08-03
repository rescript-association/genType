import * as ReactDOM from "react-dom";
import * as React from "react";

import App from "./App.gen";
import {
  printVariantWithPayload,
  testWithPayload,
} from "./VariantsWithPayload.gen";
import Hooks from "./Hooks.gen";

const consoleLog = console.log;

printVariantWithPayload("a");
printVariantWithPayload("b");
printVariantWithPayload("true");
printVariantWithPayload("Twenty");
printVariantWithPayload("Half");
printVariantWithPayload(testWithPayload({ NAME: "C", VAL: { x: 15 } }));

const Main = () => (
  <div>
    <App
      title={"hello"}
      person={{ name: "Josh", age: 33 }}
      array={["abc"]}
      callback={() => {
        console.log("callback called");
      }}
    />
    <Hooks vehicle={{ name: "auto" }} />
  </div>
);

ReactDOM.render(React.createElement(Main), document.getElementById("index"));
