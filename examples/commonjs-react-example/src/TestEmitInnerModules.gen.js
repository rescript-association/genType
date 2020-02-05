/** 
 * @flow strict
 * @generated from TestEmitInnerModules.re
 * @nolint
 */
/* eslint-disable */

// $FlowExpectedError: Reason checked type sufficiently
const TestEmitInnerModulesBS = require('./TestEmitInnerModules.bs');

const Inner_x: number = TestEmitInnerModulesBS.Inner.x;;
exports.Inner_x = Inner_x

const Inner_y: string = TestEmitInnerModulesBS.Inner.y;;
exports.Inner_y = Inner_y

const Outer_Medium_Inner_y: number = TestEmitInnerModulesBS.Outer.Medium.Inner.y;;
exports.Outer_Medium_Inner_y = Outer_Medium_Inner_y

const Inner: { x: number, y: string, ... } = TestEmitInnerModulesBS.Inner;
exports.Inner = Inner

const Outer: { Medium: { Inner: { y: number, ... }, ... }, ... } = TestEmitInnerModulesBS.Outer;
exports.Outer = Outer
