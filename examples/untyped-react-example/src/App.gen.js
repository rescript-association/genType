/** 
 * this is a custom header you can add to every file!
 * Untyped file generated from App.re by genType.
 */
/* eslint-disable */

import * as Curry from 'bs-platform/lib/es6/curry.js';

import * as PropTypes from 'prop-types';

import * as AppBS from './App.bs';

import * as ReasonReact from 'reason-react/src/ReasonReact.js';

export const App = ReasonReact.wrapReasonForJs(
  AppBS.component,
  (function _(jsProps) {
     return Curry._5(AppBS.make, jsProps.array, jsProps.callback, jsProps.person, jsProps.title, jsProps.children);
  }));

App.propTypes = {
  array : PropTypes.arrayOf(PropTypes.string).isRequired,
  callback : PropTypes.func,
  person : PropTypes.shape({
    name : PropTypes.string.isRequired,
    age : PropTypes.number.isRequired,
    optional : PropTypes.number,
    unknown : PropTypes.any
  }).isRequired,
  title : PropTypes.string.isRequired
};

export default App;

export const poly = function (Arg1, Arg2) {
  const result = Curry._2(AppBS.poly, Arg1, Arg2);
  return result
};
