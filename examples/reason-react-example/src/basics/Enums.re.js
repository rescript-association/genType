/** 
 * @flow strict
 * @generated 
 * @nolint
 */

// $FlowExpectedError: Reason checked type sufficiently
const EnumsBS = require('./Enums.bs');

const $$toRE175361521 = {"type": 449540197, "module": -134553037, "XXX THIS IS DIFFERENT": 23437694};

const $$toJS18951405 = {"449540197": "type", "-134553037": "module", "23437694": "42"};

const $$toRE396727132 = {"monday": -949852400, "tuesday": 323181965, "wednesday": -863289194, "thursday": 122883354, "friday": 835226847, "saturday": -29784519, "sunday": 569248848};

const $$toJS98879741 = {"120": "x", "26810": "same"};

const $$toRE98879741 = {"x": 120, "same": 26810};

const $$toRE18951405 = {"type": 449540197, "module": -134553037, "42": 23437694};

const $$toJS149274715 = {"120": "x", "26809": "same"};

const $$toRE916593523 = {"saturday": -29784519, "sunday": 569248848};

const $$toJS916593523 = {"-29784519": "saturday", "569248848": "sunday"};

const $$toRE149274715 = {"x": 120, "same": 26809};

const $$toJS175361521 = {"449540197": "type", "-134553037": "module", "23437694": "XXX THIS IS DIFFERENT"};

export type weekday = "monday" | "tuesday" | "wednesday" | "thursday" | "friday" | "saturday" | "sunday";

export const isWeekend: (weekday) => boolean = function _(Arg1) { const result = EnumsBS.isWeekend($$toRE396727132[Arg1]); return result };

export const monday: "monday" = "monday";

export const saturday: "saturday" = "saturday";

export const sunday: "sunday" = "sunday";

export const onlySunday: ("sunday") => void = function _(Arg1) { const result = EnumsBS.onlySunday(/* sunday */569248848); return result };

export const swap: ("saturday" | "sunday") => "saturday" | "sunday" = function _(Arg1) { const result = EnumsBS.swap($$toRE916593523[Arg1]); return $$toJS916593523[result] };

export type testGenTypeAs = "type" | "module" | "42";

export const testConvert: (testGenTypeAs) => testGenTypeAs = function _(Arg1) { const result = EnumsBS.testConvert($$toRE18951405[Arg1]); return $$toJS18951405[result] };

export const fortytwoOK: testGenTypeAs = $$toJS18951405[EnumsBS.fortytwoOK];

export const fortytwoBAD: "fortytwo" = "fortytwo";

export type testGenTypeAs2 = "type" | "module" | "42";

export const testConvert2: (testGenTypeAs2) => testGenTypeAs2 = function _(Arg1) { const result = EnumsBS.testConvert2($$toRE18951405[Arg1]); return $$toJS18951405[result] };

export type testGenTypeAs3 = "type" | "module" | "XXX THIS IS DIFFERENT";

export const testConvert3: (testGenTypeAs3) => testGenTypeAs3 = function _(Arg1) { const result = EnumsBS.testConvert3($$toRE175361521[Arg1]); return $$toJS175361521[result] };

export const testConvert2to3: (testGenTypeAs2) => testGenTypeAs3 = function _(Arg1) { const result = EnumsBS.testConvert2to3($$toRE18951405[Arg1]); return $$toJS175361521[result] };

export type x1 = "x" | "same";

export type x2 = "x" | "same";

export const id1: (x1) => x1 = function _(Arg1) { const result = EnumsBS.id1($$toRE149274715[Arg1]); return $$toJS149274715[result] };

export const id2: (x2) => x2 = function _(Arg1) { const result = EnumsBS.id2($$toRE98879741[Arg1]); return $$toJS98879741[result] };
