/* @flow strict */

const WrapJsValue = require("./WrapJsValue.re");

export const round: number => number = Math.round;

export const area = function(point: WrapJsValue.Ipoint): number {
  return point.x * (point.y === undefined ? 1 : point.y);
};

export type myArray<T> = Array<T>;

export const getValueAtIndex: (myArray<string>, number) => string = function(
  myArray: myArray<string>,
  i
) {
  return myArray[i];
};
