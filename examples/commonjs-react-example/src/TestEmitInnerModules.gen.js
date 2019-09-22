/** 
 * @flow strict
 * @generated
 * @nolint
 */
/* eslint-disable */

// $FlowExpectedError: Reason checked type sufficiently
const TestEmitInnerModulesBS = require('./TestEmitInnerModules.bs');

const Inner_x: number = TestEmitInnerModulesBS.Inner[0];;
exports.Inner_x = Inner_x

const Inner_y: string = TestEmitInnerModulesBS.Inner[1];;
exports.Inner_y = Inner_y

const Outer_Medium_Inner_y: number = TestEmitInnerModulesBS.Outer[0][0][0];;
exports.Outer_Medium_Inner_y = Outer_Medium_Inner_y

const Inner: { x: number, y: string } = { x: Inner_x, y: Inner_y };
exports.Inner = Inner

const Outer: { Medium: { Inner: { y: number } } } = { Medium: { Inner: { y: Outer_Medium_Inner_y } } };
exports.Outer = Outer
