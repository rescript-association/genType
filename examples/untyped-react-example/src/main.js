const ReactDOM = require("react-dom");
const React = require("react");

const App = require("./App.bs").default;

const Main = () => (
  <div>
    <App />
  </div>
);

ReactDOM.render(React.createElement(Main), document.getElementById("index"));
