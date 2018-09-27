import * as React from "react";
import * as ReactDOM from "react-dom";
import App from "./App";
import "./index.css";
import * as Types from "./nested/Types";
import ReasonComponent from "./ReasonComponent";
import { A, B, minus, tToString } from "./ReasonComponent";
import { t, TA, TB } from "./ReasonComponent";
import * as Records from "./Records";
import registerServiceWorker from "./registerServiceWorker";

const minusOne: number = minus({ second: 1 });

const a: TA = A;
const b: TB = B(3);
const thisIsOK: t = A;
// const thisIsATypeError: TB = A;

// tslint:disable-next-line:no-console
console.log(a, b, thisIsOK);

const intList = Types.map(x => x + 1, Types.someIntList);

const businesses = [
  {
    address: "Poison road",
    name: "AcmeLTD",
    owner: { name: "John", age: 12, address: "garage" }
  }
];

const addresses = Records.findAllAddresses(businesses);

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
