const React = require("react");

const App = props => {
  if (this.props.show) {
    return (
      <div className="App">
        {"Here's the message from the owner: " +
          (this.props.message === undefined ? "3" : this.props.message.text)}
      </div>
    );
  } else {
    return null;
  }
};

module.exports = App;
