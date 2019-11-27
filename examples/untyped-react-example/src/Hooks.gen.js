/** 
 * this is a custom header you can add to every file!
 * Untyped file generated from Hooks.re by genType.
 */

import * as React from 'react';

import * as PropTypes from 'prop-types';

import * as HooksBS from './Hooks.bs';

export const $$default = function Hooks(Arg1) {
  const $props = {vehicle:[Arg1.vehicle.name]};
  const result = React.createElement(HooksBS.default, $props);
  return result
};

$$default.propTypes = {
  vehicle : PropTypes.shape({
    name : PropTypes.string.isRequired
  }).isRequired
};

export default $$default;
