/** 
 * @flow strict
 * @generated
 * @nolint
 */
/* eslint-disable */
// $FlowExpectedError: Reason checked type sufficiently
type $any = any;

// $FlowExpectedError: Reason checked type sufficiently
const React = require('react');

// $FlowExpectedError: Reason checked type sufficiently
const HooksBS = require('./Hooks.bs');

export type vehicle = {| +name: string |};

// Type annotated function components are not checked by Flow, but typeof() works.
const $$default$$forTypeof = function (_: {| +vehicle: vehicle |}) : React$Node { return null };

export type Props = {| +vehicle: vehicle |};

const $$default: typeof($$default$$forTypeof) = function Hooks(Arg1: $any) {
  const $props = {vehicle:[Arg1.vehicle.name]};
  const result = React.createElement(HooksBS.default, $props);
  return result
};;
exports.$$default = $$default

exports.default = $$default;
