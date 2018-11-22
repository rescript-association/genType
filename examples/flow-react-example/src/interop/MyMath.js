/* @flow strict */

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
