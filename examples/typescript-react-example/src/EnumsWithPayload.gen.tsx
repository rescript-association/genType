/* TypeScript file generated by genType. */

const $$toJS542320962 = {"97": "a", "98": "bRenamed", "937218926": true, "-574635695": 20, "803296723": 0.5};

const $$toRE1058613066 = {};

const $$toJS1058613066 = {};

const $$toRE542320962 = {"a": 97, "bRenamed": 98, "true": 937218926, "20": -574635695, "0.5": 803296723};

// tslint:disable-next-line:no-var-requires
const EnumsWithPayloadBS = require('./EnumsWithPayload.bs');

// tslint:disable-next-line:interface-over-type-literal
export type payload = {readonly x: number, readonly y?: string};

// tslint:disable-next-line:interface-over-type-literal
export type withPayload = "a" | "bRenamed" | true | 20 | 0.5 | payload;

// tslint:disable-next-line:interface-over-type-literal
export type manyPayloads = {tag: "one", value: number} | {tag: "two", value: [string, string]};

export const testWithPayload: (_1:withPayload) => withPayload = function _(Arg1: any) { const result = EnumsWithPayloadBS.testWithPayload((typeof(Arg1) === 'object' ? [/* c */99, [Arg1.x, Arg1.y]] : $$toRE542320962[Arg1.toString()])); return (typeof(result) === 'object' ? {x:result[1][0], y:result[1][1]} : $$toJS542320962[result]) };

export const printEnumValue: (_1:withPayload) => void = function _(Arg1: any) { const result = EnumsWithPayloadBS.printEnumValue((typeof(Arg1) === 'object' ? [/* c */99, [Arg1.x, Arg1.y]] : $$toRE542320962[Arg1.toString()])); return result };

export const testManyPayloads: (_1:manyPayloads) => manyPayloads = function _(Arg1: any) { const result = EnumsWithPayloadBS.testManyPayloads((typeof(Arg1) === 'object' ? (Arg1.tag==="one" ? [/* one */5544550, Arg1.value] :  [/* two */5795212, Arg1.value]) : $$toRE1058613066[Arg1])); return (typeof(result) === 'object' ? (result[0]===/* one */5544550 ? {tag:"one", value:result[1]} :  {tag:"two", value:result[1]}) : $$toJS1058613066[result]) };
