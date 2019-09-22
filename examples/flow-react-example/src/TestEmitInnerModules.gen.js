/** 
 * @flow strict
 * @generated
 * @nolint
 */
/* eslint-disable */

// $FlowExpectedError: Reason checked type sufficiently
import * as TestEmitInnerModulesBS from './TestEmitInnerModules.bs';

export const Inner_x: number = TestEmitInnerModulesBS.Inner[0];

export const Inner_y: string = TestEmitInnerModulesBS.Inner[1];

export const Outer_Medium_Inner_y: number = TestEmitInnerModulesBS.Outer[0][0][0];

export const Inner = { x: Inner_x, y: Inner_y };

export const Outer = { Medium: { Inner: { y: Outer_Medium_Inner_y } } };
