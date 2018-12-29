import * as ReactDOM from "react-dom";
import * as React from "react";

import App from "./App.gen";
import { printEnumValue, testWithPayload } from "./EnumsWithPayload.gen";

const consoleLog = console.log;

printEnumValue("a");
printEnumValue("bRenamed");
printEnumValue(true);
printEnumValue(20);
printEnumValue(.5);
printEnumValue(testWithPayload({ x: 15 }));

const Main = () => (
  <div>
    <App />
  </div>
);

ReactDOM.render(React.createElement(Main), document.getElementById("index"));
