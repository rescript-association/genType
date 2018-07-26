/* @flow strict */

var ReactDOM = require('react-dom');
var React = require('react');

// Import a ReasonReact component! `jsComponent` is the exposed, underlying ReactJS class
var PageReason = require('./GreetingRe.bs').jsComponent;

var App = function() {
  return React.createElement('div', null,
    React.createElement(PageReason, {message: 'Hello!'})
  );
  // didn't feel like dragging in Babel. Here's the equivalent JSX:
  // <div><PageReason message="Hello!"></div>
};
App.displayName = 'ExampleInteropRoot';

// $FlowFixMe
ReactDOM.render(React.createElement(App), document.getElementById('index'));
