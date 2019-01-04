/* TypeScript file generated by genType. */

const YY = {"ActionNoOp": 0, "ActionAdjustAge": 1, "ActionBooly": 2, "ActionOptionalInt": 3, "ActionUnity": 4, "ActionOptionalBooly": 5, "ActionOptionalBoolMapper": 6};

// tslint:disable-next-line:no-var-requires
const VariantsBS = require('./Variants.bs');

// tslint:disable-next-line:interface-over-type-literal
export type action = "ActionNoOp" | "ActionAdjustAge" | "ActionBooly" | "ActionOptionalInt" | "ActionUnity" | "ActionOptionalBooly" | "ActionOptionalBoolMapper";

// tslint:disable-next-line:interface-over-type-literal
export type optionalBoolMapper = {readonly optionalBoolMapper: (_1:(null | undefined | boolean)) => (null | undefined | boolean)};

export const actionToString: (_1:action) => string = function _(Arg1: any) { const result = VariantsBS.actionToString(YY[Arg1]); return result };

export const converter: (_1:optionalBoolMapper) => optionalBoolMapper = function _(Arg1: any) { const result = VariantsBS.converter([function _(Arg11: any) { const result1 = Arg1.optionalBoolMapper(Arg11); return (result1 == null ? undefined : result1) }]); return {optionalBoolMapper:function _(Arg12: any) { const result2 = result[0]((Arg12 == null ? undefined : Arg12)); return result2 }} };
