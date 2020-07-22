/** 
 * @flow strict
 * @generated from Variants.re
 * @nolint
 */
/* eslint-disable */

// $FlowExpectedError: Reason checked type sufficiently
import * as VariantsBS from './Variants.bs';

export type weekday = 
    "monday"
  | "tuesday"
  | "wednesday"
  | "thursday"
  | "friday"
  | "saturday"
  | "sunday";

export type testGenTypeAs = "type" | "module" | "42";

export type testGenTypeAs2 = "type" | "module" | "42";

export type testGenTypeAs3 = "type" | "module" | "XXX THIS IS DIFFERENT";

export type x1 = "x" | "same";

export type x2 = "x" | "same";

export type type_ = "type";
export type type = type_;

export const isWeekend: (weekday) => boolean = VariantsBS.isWeekend;

export const monday: "monday" = VariantsBS.monday;

export const saturday: "saturday" = VariantsBS.saturday;

export const sunday: "sunday" = VariantsBS.sunday;

export const onlySunday: ("sunday") => void = VariantsBS.onlySunday;

export const swap: ("saturday" | "sunday") => "saturday" | "sunday" = VariantsBS.swap;

export const testConvert: (testGenTypeAs) => testGenTypeAs = VariantsBS.testConvert;

export const fortytwoOK: testGenTypeAs = VariantsBS.fortytwoOK;

export const fortytwoBAD: "fortytwo" = VariantsBS.fortytwoBAD;

export const testConvert2: (testGenTypeAs2) => testGenTypeAs2 = VariantsBS.testConvert2;

export const testConvert3: (testGenTypeAs3) => testGenTypeAs3 = VariantsBS.testConvert3;

export const testConvert2to3: (testGenTypeAs2) => testGenTypeAs3 = VariantsBS.testConvert2to3;

export const id1: (x1) => x1 = VariantsBS.id1;

export const id2: (x2) => x2 = VariantsBS.id2;

export const polyWithOpt: (string) => ?(
    {| NAME: "One", VAL: string |}
  | {| NAME: "Two", VAL: number |}) = VariantsBS.polyWithOpt;
