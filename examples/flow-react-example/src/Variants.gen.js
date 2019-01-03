/** 
 * @flow strict
 * @generated
 * @nolint
 */

const XX = {"ActionNoOp": -889096520, "ActionAdjustAge": 447703226, "ActionBooly": -309348007, "ActionOptionalInt": 909149433, "ActionUnity": -578879041, "ActionOptionalBooly": -1167463, "ActionOptionalBoolMapper": 6898881};

// $FlowExpectedError: Reason checked type sufficiently
import * as VariantsBS from './Variants.bs';

export type action = "ActionNoOp" | "ActionAdjustAge" | "ActionBooly" | "ActionOptionalInt" | "ActionUnity" | "ActionOptionalBooly" | "ActionOptionalBoolMapper";

export type optionalBoolMapper = {|+optionalBoolMapper: (?boolean) => ?boolean|};

export const actionToString: (action) => string = function _(Arg1) { const result = VariantsBS.actionToString(XX[Arg1]); return result };

export const converter: (optionalBoolMapper) => optionalBoolMapper = function _(Arg1) { const result = VariantsBS.converter([function _(Arg11) { const result1 = Arg1.optionalBoolMapper(Arg11); return (result1 == null ? undefined : result1) }]); return {optionalBoolMapper:function _(Arg12) { const result2 = result[0]((Arg12 == null ? undefined : Arg12)); return result2 }} };
