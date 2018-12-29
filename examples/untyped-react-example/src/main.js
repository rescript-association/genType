import * as ReactDOM from "react-dom";
import * as React from "react";

import App from "./App.gen";

const Main = () => (
  <div>
    <App />
  </div>
);

ReactDOM.render(React.createElement(Main), document.getElementById("index"));
