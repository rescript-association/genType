/** 
 * @flow strict
 * @generated
 * @nolint
 */

const YY = {"NoOp": 0, "AdjustAge": 1, "Booly": 2, "OptionalInt": 3, "Unity": 4, "OptionalBooly": 5, "OptionalBoolMapper": 6};

// $FlowExpectedError: Reason checked type sufficiently
import * as VariantsBS from './Variants.bs';

export type action = "NoOp" | "AdjustAge" | "Booly" | "OptionalInt" | "Unity" | "OptionalBooly" | "OptionalBoolMapper";

export type optionalBoolMapper = {|+optionalBoolMapper: (?boolean) => ?boolean|};

export const actionToString: (action) => string = function _(Arg1) { const result = VariantsBS.actionToString(YY[Arg1]); return result };

export const converter: (optionalBoolMapper) => optionalBoolMapper = function _(Arg1) { const result = VariantsBS.converter([function _(Arg11) { const result1 = Arg1.optionalBoolMapper(Arg11); return (result1 == null ? undefined : result1) }]); return {optionalBoolMapper:function _(Arg12) { const result2 = result[0]((Arg12 == null ? undefined : Arg12)); return result2 }} };
