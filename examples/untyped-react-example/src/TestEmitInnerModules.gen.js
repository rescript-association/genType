/** 
 * this is a custom header you can add to every file!
 * Untyped file generated by genType.
 */

import * as TestEmitInnerModulesBS from './TestEmitInnerModules.bs';

export const Inner_x = TestEmitInnerModulesBS.Inner[0];

export const Inner_y = TestEmitInnerModulesBS.Inner[1];

export const Outer_Medium_Inner_y = TestEmitInnerModulesBS.Outer[0][0][0];

export const Inner = { x: Inner_x, y: Inner_y }

export const Outer = { Medium: { Inner: { y: Outer_Medium_Inner_y } } }
