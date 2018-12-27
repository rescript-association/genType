/** 
 * @flow strict
 * @generated
 * @nolint
 */

const $$toRE360329949 = {"type": 449540197, "module": -134553037, "XXX THIS IS DIFFERENT": 23437694};

const $$toJS188570748 = {"120": "x", "26810": "same"};

const $$toJS663341608 = {"-29784519": "saturday", "569248848": "sunday"};

const $$toRE938418717 = {"x": 120, "same": 26809};

const $$toJS583106406 = {"449540197": "type", "-134553037": "module", "23437694": "42"};

const $$toRE583106406 = {"type": 449540197, "module": -134553037, "42": 23437694};

const $$toRE663341608 = {"saturday": -29784519, "sunday": 569248848};

const $$toRE810175546 = {"monday": -949852400, "tuesday": 323181965, "wednesday": -863289194, "thursday": 122883354, "friday": 835226847, "saturday": -29784519, "sunday": 569248848};

const $$toRE188570748 = {"x": 120, "same": 26810};

const $$toJS938418717 = {"120": "x", "26809": "same"};

const $$toJS360329949 = {"449540197": "type", "-134553037": "module", "23437694": "XXX THIS IS DIFFERENT"};

// $FlowExpectedError: Reason checked type sufficiently
import * as EnumsBS from './Enums.bs';

export type weekday = "monday" | "tuesday" | "wednesday" | "thursday" | "friday" | "saturday" | "sunday";

export type testGenTypeAs = "type" | "module" | "42";

export type testGenTypeAs2 = "type" | "module" | "42";

export type testGenTypeAs3 = "type" | "module" | "XXX THIS IS DIFFERENT";

export type x1 = "x" | "same";

export type x2 = "x" | "same";

export const isWeekend: (weekday) => boolean = function _(Arg1) { const result = EnumsBS.isWeekend($$toRE810175546[Arg1]); return result };

export const monday: "monday" = "monday";

export const saturday: "saturday" = "saturday";

export const sunday: "sunday" = "sunday";

export const onlySunday: ("sunday") => void = function _(Arg1) { const result = EnumsBS.onlySunday(/* sunday */569248848); return result };

export const swap: ("saturday" | "sunday") => "saturday" | "sunday" = function _(Arg1) { const result = EnumsBS.swap($$toRE663341608[Arg1]); return $$toJS663341608[result] };

export const testConvert: (testGenTypeAs) => testGenTypeAs = function _(Arg1) { const result = EnumsBS.testConvert($$toRE583106406[Arg1]); return $$toJS583106406[result] };

export const fortytwoOK: testGenTypeAs = $$toJS583106406[EnumsBS.fortytwoOK];

export const fortytwoBAD: "fortytwo" = "fortytwo";

export const testConvert2: (testGenTypeAs2) => testGenTypeAs2 = function _(Arg1) { const result = EnumsBS.testConvert2($$toRE583106406[Arg1]); return $$toJS583106406[result] };

export const testConvert3: (testGenTypeAs3) => testGenTypeAs3 = function _(Arg1) { const result = EnumsBS.testConvert3($$toRE360329949[Arg1]); return $$toJS360329949[result] };

export const testConvert2to3: (testGenTypeAs2) => testGenTypeAs3 = function _(Arg1) { const result = EnumsBS.testConvert2to3($$toRE583106406[Arg1]); return $$toJS360329949[result] };

export const id1: (x1) => x1 = function _(Arg1) { const result = EnumsBS.id1($$toRE938418717[Arg1]); return $$toJS938418717[result] };

export const id2: (x2) => x2 = function _(Arg1) { const result = EnumsBS.id2($$toRE188570748[Arg1]); return $$toJS188570748[result] };
