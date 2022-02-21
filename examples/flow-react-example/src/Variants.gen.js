/** 
 * @flow strict
 * @generated from Variants.res
 * @nolint
 */
/* eslint-disable */
// $FlowExpectedError[unclear-type]: Reason checked type sufficiently
type $any = any;

const $$toJS508922110 = {"type_": "type", "module_": "module", "fortytwo": "42"};

const $$toRE508922110 = {"type": "type_", "module": "module_", "42": "fortytwo"};

const $$toJS584768163 = {"type_": "type", "module_": "module", "fortytwo": "XXX THIS IS DIFFERENT"};

const $$toRE584768163 = {"type": "type_", "module": "module_", "XXX THIS IS DIFFERENT": "fortytwo"};

const $$toJS930788378 = {"x": "x", "x1": "same"};

const $$toRE930788378 = {"x": "x", "same": "x1"};

const $$toJS1061900109 = {"x": "x", "x2": "same"};

const $$toRE1061900109 = {"x": "x", "same": "x2"};

// $FlowExpectedError[untyped-import]: Reason checked type sufficiently
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

export const testConvert: (testGenTypeAs) => testGenTypeAs = function (Arg1: $any) {
  const result = VariantsBS.testConvert($$toRE508922110[Arg1]);
  return $$toJS508922110[result]
};

export const fortytwoOK: testGenTypeAs = $$toJS508922110[VariantsBS.fortytwoOK];

export const fortytwoBAD: "fortytwo" = VariantsBS.fortytwoBAD;

export const testConvert2: (testGenTypeAs2) => testGenTypeAs2 = function (Arg1: $any) {
  const result = VariantsBS.testConvert2($$toRE508922110[Arg1]);
  return $$toJS508922110[result]
};

export const testConvert3: (testGenTypeAs3) => testGenTypeAs3 = function (Arg1: $any) {
  const result = VariantsBS.testConvert3($$toRE584768163[Arg1]);
  return $$toJS584768163[result]
};

export const testConvert2to3: (testGenTypeAs2) => testGenTypeAs3 = function (Arg1: $any) {
  const result = VariantsBS.testConvert2to3($$toRE508922110[Arg1]);
  return $$toJS584768163[result]
};

export const id1: (x1) => x1 = function (Arg1: $any) {
  const result = VariantsBS.id1($$toRE930788378[Arg1]);
  return $$toJS930788378[result]
};

export const id2: (x2) => x2 = function (Arg1: $any) {
  const result = VariantsBS.id2($$toRE1061900109[Arg1]);
  return $$toJS1061900109[result]
};

export const polyWithOpt: (string) => ?(
    {| NAME: "One", VAL: string |}
  | {| NAME: "Two", VAL: number |}) = VariantsBS.polyWithOpt;
