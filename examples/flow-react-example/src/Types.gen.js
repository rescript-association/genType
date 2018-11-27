/** 
 * @flow strict
 * @generated
 * @nolint
 */

// flowlint-next-line nonstrict-import:off
import {foo as fooNotChecked} from './name-with-dashes';

// In case of type error, check the type of 'foo' in 'Types.re' and './name-with-dashes'.
export const fooTypeChecked: (number) => number = fooNotChecked;

// Export 'foo' early to allow circular import from the '.bs.js' file.
export const foo: mixed = fooTypeChecked;

// $FlowExpectedError: Reason checked type sufficiently
const CreateBucklescriptBlock = require('bs-platform/lib/es6/block.js');

// $FlowExpectedError: Reason checked type sufficiently
const TypesBS = require('./Types.bs');

// flowlint-next-line nonstrict-import:off
import type {anInterestingFlowType} from './SomeFlowTypes';

// flowlint-next-line nonstrict-import:off
import type {annotatedVariant as AutoAnnotate_annotatedVariant} from './AutoAnnotate.gen';

// flowlint-next-line nonstrict-import:off
import type {t as Obj_t} from '../src/shims/Obj.shim';

// flowlint-next-line nonstrict-import:off
import type {variant as AutoAnnotate_variant} from './AutoAnnotate.gen';

// flowlint-next-line nonstrict-import:off
import type {weekday} from './SomeFlowTypes';

export opaque type TypeWithVarsA<x,y> = mixed;

export const A: <x,y>(x, y) => TypeWithVarsA<x,y> = function _(VArg1, VArg2) { return CreateBucklescriptBlock.__(0, [VArg1, VArg2]) }

export opaque type TypeWithVarsB<z> = mixed;

export const B: <z>(z) => TypeWithVarsB<z> = function _(VArg1) { return CreateBucklescriptBlock.__(1, [VArg1]) }

export type typeWithVars<x,y,z> =
  | TypeWithVarsA<x,y>
  | TypeWithVarsB<z>;

export type optionInt = ?number;

export type funType = (number) => number;

export type myFloat = number;

export type arrayOfStrings1 = Array<string>;

export type arrayOfStrings2 = Array<string>;

export type maybeString = ?string;

export type maybeString2 = ?string;

export type peopleArray = Array<{|+name: string, +nickname: ?string|}>;

export opaque type myObj = mixed;

export type { anInterestingFlowType };

export type { weekday };

export type someMutableFields = {|
  mutable0: string, 
  +immutable: number, 
  mutable1: string, 
  mutable2: string
|};

export opaque type exportOpaqueFromEnums = mixed;

export type dateKey = string;
export type DateKey = dateKey;

export opaque type dateKeyOpaque = mixed;
export type DateKeyOpaque = dateKeyOpaque;

export const consumeOption: (?number) => number = function _(Arg1) { const result = TypesBS.consumeOption((Arg1 == null ? undefined : Arg1)); return result };

export const consumeOption2: (optionInt) => number = function _(Arg1) { const result = TypesBS.consumeOption2((Arg1 == null ? undefined : Arg1)); return result };

export const testArray: (Array<?number>) => Array<?number> = function _(Arg1) { const result = TypesBS.testArray(Arg1.map(function _element(x) { return (x == null ? undefined : x)})); return result };

export const identity: (anInterestingFlowType) => anInterestingFlowType = TypesBS.identity;

export const isWeekend: (weekday) => boolean = TypesBS.isWeekend;

export const testFunctionOnOptionsAsArgument: <T1,a>(?a, ((?a) => T1)) => T1 = function _(Arg1, Arg2) { const result = TypesBS.testFunctionOnOptionsAsArgument((Arg1 == null ? undefined : Arg1), Arg2); return result };

export const testDateKey: (dateKey) => dateKey = TypesBS.testDateKey;

export const testAutoAnnotateVariants: (AutoAnnotate_variant) => AutoAnnotate_variant = TypesBS.testAutoAnnotateVariants;

export const testAutoAnnotateVariants2: (AutoAnnotate_annotatedVariant) => AutoAnnotate_annotatedVariant = TypesBS.testAutoAnnotateVariants2;
