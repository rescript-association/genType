import * as ReactDOM from "react-dom";
import * as React from "react";

import App from "./App.gen";
import { printVariantWithPayload, testWithPayload } from "./VariantsWithPayload.gen";

const consoleLog = console.log;

printVariantWithPayload("a");
printVariantWithPayload("bRenamed");
printVariantWithPayload(true);
printVariantWithPayload(20);
printVariantWithPayload(.5);
printVariantWithPayload(testWithPayload({ x: 15 }));

const Main = () => (
  <div>
    <App />
  </div>
);

ReactDOM.render(React.createElement(Main), document.getElementById("index"));
