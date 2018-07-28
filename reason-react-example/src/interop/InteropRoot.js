/* @flow strict */

const ReactDOM = require('react-dom');
const React = require('react');

const GreetingRe = require('./GreetingRe.re');

// Import a ReasonReact component!
const PageReason = GreetingRe.component;

const helloWorldList = GreetingRe.cons("Hello", GreetingRe.cons("World", GreetingRe.empty));

const helloWorld = GreetingRe.concat("++", helloWorldList);

const App = () => <div> <PageReason message={helloWorld}/> </div>;
App.displayName = 'ExampleInteropRoot';

// $FlowFixMe
ReactDOM.render(React.createElement(App), document.getElementById('index'));
