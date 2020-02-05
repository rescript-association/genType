/** 
 * @flow strict
 * @generated from TestEmitInnerModules.re
 * @nolint
 */
/* eslint-disable */

// $FlowExpectedError: Reason checked type sufficiently
import * as TestEmitInnerModulesBS from './TestEmitInnerModules.bs';

export const Inner_x: number = TestEmitInnerModulesBS.Inner.x;

export const Inner_y: string = TestEmitInnerModulesBS.Inner.y;

export const Outer_Medium_Inner_y: number = TestEmitInnerModulesBS.Outer.Medium.Inner.y;

export const Inner: { x: number, y: string, ... } = TestEmitInnerModulesBS.Inner

export const Outer: { Medium: { Inner: { y: number, ... }, ... }, ... } = TestEmitInnerModulesBS.Outer
