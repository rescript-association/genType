/** 
 * @flow strict
 * @generated
 * @nolint
 */

const YY = {"ActionNoOp": 0, "ActionAdjustAge": 1, "ActionBooly": 2, "ActionOptionalInt": 3, "ActionUnity": 4, "ActionOptionalBooly": 5, "ActionOptionalBoolMapper": 6};

// $FlowExpectedError: Reason checked type sufficiently
import * as VariantsBS from './Variants.bs';

export type action = "ActionNoOp" | "ActionAdjustAge" | "ActionBooly" | "ActionOptionalInt" | "ActionUnity" | "ActionOptionalBooly" | "ActionOptionalBoolMapper";

export type optionalBoolMapper = {|+optionalBoolMapper: (?boolean) => ?boolean|};

export const actionToString: (action) => string = function _(Arg1) { const result = VariantsBS.actionToString(YY[Arg1]); return result };

export const converter: (optionalBoolMapper) => optionalBoolMapper = function _(Arg1) { const result = VariantsBS.converter([function _(Arg11) { const result1 = Arg1.optionalBoolMapper(Arg11); return (result1 == null ? undefined : result1) }]); return {optionalBoolMapper:function _(Arg12) { const result2 = result[0]((Arg12 == null ? undefined : Arg12)); return result2 }} };
