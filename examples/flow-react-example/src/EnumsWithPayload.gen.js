/** 
 * @flow strict
 * @generated
 * @nolint
 */

const $$toJS542320962 = {"97": "a", "98": "bRenamed", "937218926": true, "-574635695": 20, "803296723": 0.5};

const $$toRE542320962 = {"a": 97, "bRenamed": 98, "true": 937218926, "20": -574635695, "0.5": 803296723};

// $FlowExpectedError: Reason checked type sufficiently
import * as EnumsWithPayloadBS from './EnumsWithPayload.bs';

export type payload = {|+x: number, +y?: string|};

export type withPayload = "a" | "bRenamed" | true | 20 | 0.5 | payload;

export type manyPayloads = {|tag: "one", value: number|} | {|tag: "two", value: [string, string]|} | {|tag: "three", value: payload|};

export const testWithPayload: (withPayload) => withPayload = function _(Arg1) { const result = EnumsWithPayloadBS.testWithPayload((typeof(Arg1) === 'object' ? [/* c */99, [Arg1.x, Arg1.y]] : $$toRE542320962[Arg1.toString()])); return (typeof(result) === 'object' ? {x:result[1][0], y:result[1][1]} : $$toJS542320962[result]) };

export const printEnumValue: (withPayload) => void = function _(Arg1) { const result = EnumsWithPayloadBS.printEnumValue((typeof(Arg1) === 'object' ? [/* c */99, [Arg1.x, Arg1.y]] : $$toRE542320962[Arg1.toString()])); return result };

export const testManyPayloads: (manyPayloads) => manyPayloads = function _(Arg1) { const result = EnumsWithPayloadBS.testManyPayloads((Arg1.tag==="one" ? [/* one */5544550, Arg1.value] :  Arg1.tag==="two" ? [/* two */5795212, Arg1.value] :  [/* three */261117022, [Arg1.value.x, Arg1.value.y]])); return (result[0]===/* one */5544550 ? {tag:"one", value:result[1]} :  result[0]===/* two */5795212 ? {tag:"two", value:result[1]} :  {tag:"three", value:{x:result[1][0], y:result[1][1]}}) };

export const printManyPayloads: (manyPayloads) => void = function _(Arg1) { const result = EnumsWithPayloadBS.printManyPayloads((Arg1.tag==="one" ? [/* one */5544550, Arg1.value] :  Arg1.tag==="two" ? [/* two */5795212, Arg1.value] :  [/* three */261117022, [Arg1.value.x, Arg1.value.y]])); return result };
