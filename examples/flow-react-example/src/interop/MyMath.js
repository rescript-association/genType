/* @flow strict */

// $FlowExpectedError: Reason checked type sufficiently
type $any = any;

const ImportJsValue = require("./ImportJsValue.gen");

export const round: number => number = Math.round;

export const area = function(point: ImportJsValue.point): number {
  return point.x * (point.y === undefined ? 1 : point.y);
};

export type myArray<T> = Array<T>;

export const getValueAtIndex: (myArray<string>, number) => string = function(
  myArray: myArray<string>,
  i
) {
  return myArray[i];
};

export const functionWithRenamedArgument: (
  string,
  {|
    +ArgRenamed: string
  |}
) => string = function(_) {
  return "";
};

export const useColor = function(x: "tomato" | "gray"): number {
  return 0;
};

export const higherOrder = (foo: (_1: number, _2: number) => number) =>
  foo(3, 4);

export const convertVariant = (x: $any) => x;

export class AbsoluteValue {
  prop: number;
  getProp(): number {
    return this.prop;
  }
  getAbs(): number {
    return this.prop < 0 ? -this.prop : this.prop;
  }
}

export type stringFunction = (_: string) => string;

export const polymorphic = <T>(x: T): T => x;

export type num = number;

export type polyType<T> = {x:T};

export default 42;
