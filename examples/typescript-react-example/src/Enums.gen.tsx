/* TypeScript file generated by genType. */

const $$toJS1061900109 = {"120": "x", "26810": "same"};

const $$toRE694113598 = {"saturday": -29784519, "sunday": 569248848};

const $$toRE1061900109 = {"x": 120, "same": 26810};

const $$toJS584768163 = {"449540197": "type", "-134553037": "module", "23437694": "XXX THIS IS DIFFERENT"};

const $$toJS930788378 = {"120": "x", "26809": "same"};

const $$toJS508922110 = {"449540197": "type", "-134553037": "module", "23437694": "42"};

const $$toRE930788378 = {"x": 120, "same": 26809};

const $$toRE508922110 = {"type": 449540197, "module": -134553037, "42": 23437694};

const $$toRE584768163 = {"type": 449540197, "module": -134553037, "XXX THIS IS DIFFERENT": 23437694};

const $$toJS694113598 = {"-29784519": "saturday", "569248848": "sunday"};

const $$toRE288839514 = {"monday": -949852400, "tuesday": 323181965, "wednesday": -863289194, "thursday": 122883354, "friday": 835226847, "saturday": -29784519, "sunday": 569248848};

// tslint:disable-next-line:no-var-requires
const EnumsBS = require('./Enums.bs');

// tslint:disable-next-line:interface-over-type-literal
export type weekday = "monday" | "tuesday" | "wednesday" | "thursday" | "friday" | "saturday" | "sunday";

// tslint:disable-next-line:interface-over-type-literal
export type testGenTypeAs = "type" | "module" | "42";

// tslint:disable-next-line:interface-over-type-literal
export type testGenTypeAs2 = "type" | "module" | "42";

// tslint:disable-next-line:interface-over-type-literal
export type testGenTypeAs3 = "type" | "module" | "XXX THIS IS DIFFERENT";

// tslint:disable-next-line:interface-over-type-literal
export type x1 = "x" | "same";

// tslint:disable-next-line:interface-over-type-literal
export type x2 = "x" | "same";

// tslint:disable-next-line:interface-over-type-literal
export type type_ = "type";
export type type = type_;

export const isWeekend: (_1:weekday) => boolean = function _(Arg1: any) { const result = EnumsBS.isWeekend($$toRE288839514[Arg1]); return result };

export const monday: "monday" = "monday";

export const saturday: "saturday" = "saturday";

export const sunday: "sunday" = "sunday";

export const onlySunday: (_1:"sunday") => void = function _(Arg1: any) { const result = EnumsBS.onlySunday(/* sunday */569248848); return result };

export const swap: (_1:"saturday" | "sunday") => "saturday" | "sunday" = function _(Arg1: any) { const result = EnumsBS.swap($$toRE694113598[Arg1]); return $$toJS694113598[result] };

export const testConvert: (_1:testGenTypeAs) => testGenTypeAs = function _(Arg1: any) { const result = EnumsBS.testConvert($$toRE508922110[Arg1]); return $$toJS508922110[result] };

export const fortytwoOK: testGenTypeAs = $$toJS508922110[EnumsBS.fortytwoOK];

export const fortytwoBAD: "fortytwo" = "fortytwo";

export const testConvert2: (_1:testGenTypeAs2) => testGenTypeAs2 = function _(Arg1: any) { const result = EnumsBS.testConvert2($$toRE508922110[Arg1]); return $$toJS508922110[result] };

export const testConvert3: (_1:testGenTypeAs3) => testGenTypeAs3 = function _(Arg1: any) { const result = EnumsBS.testConvert3($$toRE584768163[Arg1]); return $$toJS584768163[result] };

export const testConvert2to3: (_1:testGenTypeAs2) => testGenTypeAs3 = function _(Arg1: any) { const result = EnumsBS.testConvert2to3($$toRE508922110[Arg1]); return $$toJS584768163[result] };

export const id1: (_1:x1) => x1 = function _(Arg1: any) { const result = EnumsBS.id1($$toRE930788378[Arg1]); return $$toJS930788378[result] };

export const id2: (_1:x2) => x2 = function _(Arg1: any) { const result = EnumsBS.id2($$toRE1061900109[Arg1]); return $$toJS1061900109[result] };
