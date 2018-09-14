// @flow strict

// This file isn't used directly by JS; it's used to myBanner.re, which is then
// used by the ReasonReact component GreetingRe.

var ReactDOM = require('react-dom');
var React = require('react');

export type Props = {show:boolean, message?:string}

class App extends React.Component<Props> {
  render() {
    if (this.props.show) {
      return React.createElement('div', null,
        'Here\'s the message from the owner: ' + (this.props.message || "3")
      );
    } else {
      return null;
    }
  
  }
}

module.exports = App ;
