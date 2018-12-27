/** 
 * @flow strict
 * @generated
 * @nolint
 */

const $$toJS636506023 = {"97": "a", "98": "bRenamed", "937218926": true, "-574635695": 20};

const $$toRE636506023 = {"a": 97, "bRenamed": 98, "true": 937218926, "20": -574635695};

// $FlowExpectedError: Reason checked type sufficiently
import * as EnumsWithPayloadBS from './EnumsWithPayload.bs';

export type payload = {|+x: number, +y?: string|};

export type withPayload = "a" | "bRenamed" | true | 20 | payload;

export const testWithPayload: (withPayload) => withPayload = function _(Arg1) { const result = EnumsWithPayloadBS.testWithPayload((typeof(Arg1) === 'object' ? [/* c */99, [Arg1.x, Arg1.y]] : $$toRE636506023[Arg1.toString()])); return (typeof(result) === 'object' ? {x:result[1][0], y:result[1][1]} : $$toJS636506023[result]) };
