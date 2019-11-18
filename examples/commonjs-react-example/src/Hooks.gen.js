/** 
 * @flow strict
 * @generated from Hooks.re
 * @nolint
 */
/* eslint-disable */

// $FlowExpectedError: Reason checked type sufficiently
const HooksBS = require('./Hooks.bs');

export type vehicle = {| +name: string |};

// Type annotated function components are not checked by Flow, but typeof() works.
const $$default$$forTypeof = function (_: {| +vehicle: vehicle |}) : React$Node { return null };

export type Props = {| +vehicle: vehicle |};

const $$default: typeof($$default$$forTypeof) = HooksBS.default;;
exports.$$default = $$default

exports.default = $$default;
