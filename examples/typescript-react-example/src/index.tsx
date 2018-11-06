import * as React from "react";
import * as ReactDOM from "react-dom";
import App from "./App";
import * as Enums from "./Enums";
import "./index.css";
import * as MyMath from "./MyMath";
import * as Records from "./nested/Records";
import * as Types from "./nested/Types";
import { Universe_Nested2_Nested3_nested3Value } from "./NestedModules";
import ReasonComponent from "./ReasonComponent";
import { A, B, minus, tToString } from "./ReasonComponent";
import { t, TA, TB } from "./ReasonComponent";
import registerServiceWorker from "./registerServiceWorker";
import * as WrapJsValue from "./WrapJsValue";

const minusOne: number = minus({ second: 1 });

const a: TA = A;
const b: TB = B(3);
const thisIsOK: t = A;
// const thisIsATypeError: TB = A;

// tslint:disable-next-line:no-console
const consoleLog = console.log;

consoleLog(a, b, thisIsOK);

const intList = Types.map(x => x + 1, Types.someIntList);

const businesses = [
  {
    address: "Poison road",
    name: "AcmeLTD",
    owner: { name: "John", age: 12, address: "garage" }
  }
];

const addresses = Records.findAllAddresses(businesses);

import { roundedNumber } from "./WrapJsValue";
consoleLog("index.tsx roundedNumber:", roundedNumber);
import { areaValue } from "./WrapJsValue";
consoleLog("index.tsx areaValue:", areaValue);

ReactDOM.render(
  <div>
    <App name={"Hello"} />
    <ReasonComponent
      message={
        "Message from typescript: minus one is " +
        minusOne +
        " and B(3) prints as " +
        tToString(b) +
        " addresses: " +
        addresses
      }
      intList={intList}
      person={{
        name: "Name",
        polymorphicPayload: null,
        surname: "Surname",
        type: ""
      }}
    />
  </div>,
  document.getElementById("root") as HTMLElement
);
registerServiceWorker();

const x1 = Records.getPayload(Records.payloadValue).v;
const x2 = Records.getPayloadRecord(Records.payloadValue).v;
const x3 = Records.payloadValue.payload.v;
const x4 = Records.getPayloadRecordPlusOne(Records.payloadValue).v;
consoleLog("x1,x2,x3,x4 are", x1, x2, x3, x4);

consoleLog(
  "Universe_Nested2_Nested3_nested3Value: ",
  Universe_Nested2_Nested3_nested3Value
);

consoleLog("Enums: swap(sunday) =", Enums.swap("sunday"));
consoleLog("Enums: fortytwoOK is", Enums.fortytwoOK);
consoleLog("Enums: fortytwoBAD is", Enums.fortytwoBAD);
consoleLog(
  "Enums: testConvert3to2('module') =",
  Enums.testConvert2to3("module")
);
consoleLog("Enums: testConvert3to2('42') =", Enums.testConvert2to3("42"));

const absoluteValueInstance = new MyMath.AbsoluteValue();
absoluteValueInstance.prop = -3;
consoleLog("absoluteValueInstance", absoluteValueInstance);

const propValue = WrapJsValue.useGetProp(absoluteValueInstance);
const absValue = WrapJsValue.useGetAbs(absoluteValueInstance);
consoleLog("WrapJsValue: getProp() =", propValue);
consoleLog("WrapJsValue: getAbs() =", absValue);
