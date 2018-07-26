/* @flow strict */

var ReactDOM = require('react-dom');
var React = require('react');

// Import a ReasonReact component! `jsComponent` is the exposed, underlying ReactJS class
var PageReason = require('./GreetingRe.re').component;

var App = () => <div> <PageReason message="Hello!"/> </div>;
App.displayName = 'ExampleInteropRoot';

// $FlowFixMe
ReactDOM.render(React.createElement(App), document.getElementById('index'));
