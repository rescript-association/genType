/** 
 * @flow strict
 * @generated
 * @nolint
 */

// $FlowExpectedError: Reason checked type sufficiently
import * as CreateBucklescriptBlock from 'bs-platform/lib/es6/block.js';

// $FlowExpectedError: Reason checked type sufficiently
import * as VariantsBS from './Variants.bs';

export opaque type ActionNoOp = mixed;

export const NoOp: ActionNoOp = 0;

export opaque type ActionAdjustAge = mixed;

export const AdjustAge: (((number) => number)) => ActionAdjustAge = function _(VArg1) { return CreateBucklescriptBlock.__(0, [VArg1]) }

export opaque type ActionBooly = mixed;

export const Booly: (boolean) => ActionBooly = function _(VArg1) { return CreateBucklescriptBlock.__(1, [VArg1]) }

export opaque type ActionOptionalInt = mixed;

export const OptionalInt: (?number) => ActionOptionalInt = function _(VArg1) { return CreateBucklescriptBlock.__(2, [(VArg1 == null ? undefined : VArg1)]) }

export opaque type ActionUnity = mixed;

export const Unity: (void) => ActionUnity = function _(VArg1) { return CreateBucklescriptBlock.__(3, [VArg1]) }

export opaque type ActionOptionalBooly = mixed;

export const OptionalBooly: (?boolean) => ActionOptionalBooly = function _(VArg1) { return CreateBucklescriptBlock.__(4, [(VArg1 == null ? undefined : VArg1)]) }

export opaque type ActionOptionalBoolMapper = mixed;

export const OptionalBoolMapper: (((?boolean) => ?boolean)) => ActionOptionalBoolMapper = function _(VArg1) { return CreateBucklescriptBlock.__(5, [function _(Arg1) { const result = VArg1(Arg1); return (result == null ? undefined : result) }]) }

export type action =
  | ActionNoOp
  | ActionAdjustAge
  | ActionBooly
  | ActionOptionalInt
  | ActionUnity
  | ActionOptionalBooly
  | ActionOptionalBoolMapper;

export type optionalBoolMapper = {|+optionalBoolMapper: (?boolean) => ?boolean|};

export const actionToString: (action) => string = VariantsBS.actionToString;

export const converter: (optionalBoolMapper) => optionalBoolMapper = function _(Arg1) { const result = VariantsBS.converter([function _(Arg11) { const result1 = Arg1.optionalBoolMapper(Arg11); return (result1 == null ? undefined : result1) }]); return {optionalBoolMapper:function _(Arg12) { const result2 = result[0]((Arg12 == null ? undefined : Arg12)); return result2 }} };
