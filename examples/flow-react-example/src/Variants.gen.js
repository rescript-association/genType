/** 
 * @flow strict
 * @generated
 * @nolint
 */

const $$toRE102731832 = {"NoOp": 0};

// $FlowExpectedError: Reason checked type sufficiently
import * as VariantsBS from './Variants.bs';

export type action = "NoOp" | {|tag: "AdjustAge", value: [(number) => number]|} | {|tag: "Booly", value: [boolean]|} | {|tag: "OptionalInt", value: [?number]|} | {|tag: "Unity", value: [void]|} | {|tag: "OptionalBooly", value: [?boolean]|} | {|tag: "OptionalBoolMapper", value: [(?boolean) => ?boolean]|};

export type optionalBoolMapper = {|+optionalBoolMapper: (?boolean) => ?boolean|};

export const actionToString: (action) => string = function _(Arg1) { const result = VariantsBS.actionToString((typeof(Arg1) === 'object' ? (Arg1.tag==="AdjustAge" ? CreateBucklescriptBlock.__(0, Arg1.value) :  Arg1.tag==="Booly" ? CreateBucklescriptBlock.__(1, Arg1.value) :  Arg1.tag==="OptionalInt" ? CreateBucklescriptBlock.__(2, [(Arg1.value[0] == null ? undefined : Arg1.value[0])]) :  Arg1.tag==="Unity" ? CreateBucklescriptBlock.__(3, Arg1.value) :  Arg1.tag==="OptionalBooly" ? CreateBucklescriptBlock.__(4, [(Arg1.value[0] == null ? undefined : Arg1.value[0])]) :  CreateBucklescriptBlock.__(5, [function _(Arg11) { const result1 = Arg1.value[0](Arg11); return (result1 == null ? undefined : result1) }])) : $$toRE102731832[Arg1])); return result };

export const converter: (optionalBoolMapper) => optionalBoolMapper = function _(Arg1) { const result = VariantsBS.converter([function _(Arg11) { const result1 = Arg1.optionalBoolMapper(Arg11); return (result1 == null ? undefined : result1) }]); return {optionalBoolMapper:function _(Arg12) { const result2 = result[0]((Arg12 == null ? undefined : Arg12)); return result2 }} };
