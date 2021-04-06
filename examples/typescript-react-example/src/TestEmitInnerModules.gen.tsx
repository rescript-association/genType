/* TypeScript file generated from TestEmitInnerModules.res by genType. */
/* eslint-disable import/first */


// @ts-ignore: Implicit any on import
import * as TestEmitInnerModulesBS from './TestEmitInnerModules.bs';

export const Inner_x: number = TestEmitInnerModulesBS.Inner.x;

export const Inner_y: string = TestEmitInnerModulesBS.Inner.y;

export const Outer_Medium_Inner_y: number = TestEmitInnerModulesBS.Outer.Medium.Inner.y;

export const Inner: { x: number; y: string } = TestEmitInnerModulesBS.Inner

export const Outer: { Medium: { Inner: { y: number } } } = TestEmitInnerModulesBS.Outer
