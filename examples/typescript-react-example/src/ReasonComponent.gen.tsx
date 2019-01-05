/* TypeScript file generated by genType. */

const YY = {"A": 0};

// tslint:disable-next-line:no-var-requires
const Curry = require('bs-platform/lib/es6/curry.js');

// tslint:disable-next-line:no-var-requires
const ReasonComponentBS = require('./ReasonComponent.bs');

// tslint:disable-next-line:no-var-requires
const ReasonReact = require('reason-react/src/ReasonReact.js');

import {Mouse_t as ReactEvent_Mouse_t} from '../src/shims/ReactEvent.shim';

import {coord as Records_coord} from '../src/nested/Records.gen';

import {list} from '../src/shims/ReasonPervasives.shim';

import {t as Types_t} from '../src/nested/Types.gen';

// tslint:disable-next-line:interface-over-type-literal
export type person<a> = {
  readonly name: string, 
  readonly surname: string, 
  readonly type: string, 
  readonly polymorphicPayload: a
};

// tslint:disable-next-line:interface-over-type-literal
export type t = "A" | {tag: "B", value: number} | {tag: "C", value: string};

export const onClick: (_1:ReactEvent_Mouse_t) => void = ReasonComponentBS.onClick;

// tslint:disable-next-line:interface-over-type-literal
export type Props = {
  readonly message?: string, 
  readonly person: person<unknown>, 
  readonly intList?: list<number>, 
  readonly children?: unknown
};

export const ReasonComponent: React.ComponentClass<Props> = ReasonReact.wrapReasonForJs(
  ReasonComponentBS.component,
  (function _(jsProps: Props) {
     return Curry._4(ReasonComponentBS.make, jsProps.message, [jsProps.person.name, jsProps.person.surname, jsProps.person.type, jsProps.person.polymorphicPayload], jsProps.intList, jsProps.children);
  }));

export default ReasonComponent;

export const minus: (_1:{readonly first?: number, readonly second: number}) => number = function _(Arg1: any) { const result = ReasonComponentBS.minus(Arg1.first, Arg1.second); return result };

export const useTypeDefinedInAnotherModule: (_1:Types_t) => Types_t = ReasonComponentBS.useTypeDefinedInAnotherModule;

export const tToString: (_1:t) => string = function _(Arg1: any) { const result = ReasonComponentBS.tToString((typeof(Arg1) === 'object' ? (Arg1.tag==="B" ? [/* 0 */48, Arg1.value] :  [/* 1 */49, Arg1.value]) : YY[Arg1])); return result };

export const useRecordsCoord: (_1:Records_coord) => number = function _(Arg1: any) { const result = ReasonComponentBS.useRecordsCoord([Arg1.x, Arg1.y, Arg1.z]); return result };
