/* @flow strict */

var ReactDOM = require('react-dom');
var React = require('react');

// Import a ReasonReact component! `jsComponent` is the exposed, underlying ReactJS class
var PageReason = require('./GreetingRe.re.js').component;

var App = () => <div> <PageReason message="This is from genFlow!"/> </div>;
App.displayName = 'ExampleInteropRoot';

// $FlowFixMe
ReactDOM.render(React.createElement(App), document.getElementById('index'));
