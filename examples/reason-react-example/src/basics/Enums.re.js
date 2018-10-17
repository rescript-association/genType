/** 
 * @flow strict
 * @generated 
 * @nolint
 */

// $FlowExpectedError: Reason checked type sufficiently
const EnumsBS = require('./Enums.bs');

const $$toJS66407473 = {"449540197": "type", "-134553037": "module", "23437694": "XXX THIS IS DIFFERENT"};

const $$toJS329149378 = {"449540197": "type", "-134553037": "module", "23437694": "42"};

const $$toJS656597115 = {"-29784519": "saturday", "569248848": "sunday"};

const $$toRE528274187 = {"monday": -949852400, "tuesday": 323181965, "wednesday": -863289194, "thursday": 122883354, "friday": 835226847, "saturday": -29784519, "sunday": 569248848};

const $$toRE66407473 = {"type": 449540197, "module": -134553037, "XXX THIS IS DIFFERENT": 23437694};

const $$toRE329149378 = {"type": 449540197, "module": -134553037, "42": 23437694};

const $$toRE656597115 = {"saturday": -29784519, "sunday": 569248848};

export type weekday = "monday" | "tuesday" | "wednesday" | "thursday" | "friday" | "saturday" | "sunday";

export const isWeekend: (weekday) => boolean = function _(Arg1) { const result = EnumsBS.isWeekend($$toRE528274187[Arg1]); return result };

export const monday: "monday" = "monday";

export const saturday: "saturday" = "saturday";

export const sunday: "sunday" = "sunday";

export const onlySunday: ("sunday") => void = function _(Arg1) { const result = EnumsBS.onlySunday(/* sunday */569248848); return result };

export const swap: ("saturday" | "sunday") => "saturday" | "sunday" = function _(Arg1) { const result = EnumsBS.swap($$toRE656597115[Arg1]); return $$toJS656597115[result] };

export type testGenTypeAs = "type" | "module" | "42";

export const testConvert: (testGenTypeAs) => testGenTypeAs = function _(Arg1) { const result = EnumsBS.testConvert($$toRE329149378[Arg1]); return $$toJS329149378[result] };

export const fortytwoOK: testGenTypeAs = $$toJS329149378[EnumsBS.fortytwoOK];

export const fortytwoBAD: "fortytwo" = "fortytwo";

export type testGenTypeAs2 = "type" | "module" | "42";

export const testConvert2: (testGenTypeAs2) => testGenTypeAs2 = function _(Arg1) { const result = EnumsBS.testConvert2($$toRE329149378[Arg1]); return $$toJS329149378[result] };

export type testGenTypeAs3 = "type" | "module" | "XXX THIS IS DIFFERENT";

export const testConvert3: (testGenTypeAs3) => testGenTypeAs3 = function _(Arg1) { const result = EnumsBS.testConvert3($$toRE66407473[Arg1]); return $$toJS66407473[result] };

export const testConvert2to3: (testGenTypeAs2) => testGenTypeAs3 = function _(Arg1) { const result = EnumsBS.testConvert2to3($$toRE329149378[Arg1]); return $$toJS66407473[result] };
