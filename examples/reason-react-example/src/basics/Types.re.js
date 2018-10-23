/** 
 * @flow strict
 * @generated 
 * @nolint
 */

// $FlowExpectedError: Reason checked type sufficiently
const CreateBucklescriptBlock = require('bs-platform/lib/js/block.js');

// $FlowExpectedError: Reason checked type sufficiently
const TypesBS = require('./Types.bs');

import type {Array_t as Js_Array_t} from '../../src/shims/Js.shim';

import type {anInterestingFlowType} from './SomeFlowTypes';

import type {t as Obj_t} from '../../src/shims/Obj.shim';

import type {weekday} from './SomeFlowTypes';

export opaque type TypeWithVarsA<x,y> = mixed;

export const A: <x,y>(x, y) => TypeWithVarsA<x,y> = function _(Arg1, Arg2) { return CreateBucklescriptBlock.__(0, [Arg1, Arg2]) }

export opaque type TypeWithVarsB<z> = mixed;

export const B: <z>(z) => TypeWithVarsB<z> = function _(Arg1) { return CreateBucklescriptBlock.__(1, [Arg1]) }

export type typeWithVars<x,y,z> =
  | TypeWithVarsA<x,y>
  | TypeWithVarsB<z>;

export type optionInt = ?number;

export type funType = (number) => number;

export type myFloat = number;

export type arrayOfStrings1 = Array<string>;

export opaque type arrayOfStrings2 = mixed;

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

export const consumeOption: (?number) => number = function _(Arg1) { const result = TypesBS.consumeOption((Arg1 == null ? undefined : Arg1)); return result };

export const consumeOption2: (optionInt) => number = function _(Arg1) { const result = TypesBS.consumeOption2((Arg1 == null ? undefined : Arg1)); return result };

export const testArray: (Array<?number>) => Array<?number> = function _(Arg1) { const result = TypesBS.testArray(Arg1.map(function _element(x) { return (x == null ? undefined : x)})); return result };

export const identity: (anInterestingFlowType) => anInterestingFlowType = TypesBS.identity;

export const isWeekend: (weekday) => boolean = TypesBS.isWeekend;

export const testFunctionOnOptionsAsArgument: <T1,a>(?a, ((?a) => T1)) => T1 = function _(Arg1, Arg2) { const result = TypesBS.testFunctionOnOptionsAsArgument((Arg1 == null ? undefined : Arg1), Arg2); return result };
