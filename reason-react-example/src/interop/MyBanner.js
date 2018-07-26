// This file isn't used directly by JS; it's used to myBanner.re, which is then
// used by the ReasonReact component GreetingRe.

var ReactDOM = require('react-dom');
var React = require('react');

var App = function(props) {
  if (props.show) {
    return React.createElement('div', null,
      'Here\'s the message from the owner: ' + props.message
    );
  } else {
    return null;
  }
};
App.displayName = "MyBanner";

module.exports = App;
