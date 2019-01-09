// @flow

// This file isn't used directly by JS; it's used to myBanner.re, which is then
// used by the ReasonReact component GreetingRe.

const ReactDOM = require("react-dom");
const React = require("react");

export type Props = {| show: boolean, Message: ?string |};

class MyBanner extends React.Component<Props> {
  render() {
    if (this.props.show) {
      return React.createElement(
        "div",
        null,
        "Here's the message from App in JS: " +
          (this.props.Message != null ? this.props.Message : "3")
      );
    } else {
      return null;
    }
  }
}

module.exports = MyBanner;
