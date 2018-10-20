/* @flow strict */

const WrapJsValue = require("./WrapJsValue.re");

export const round: number => number = Math.round;

export const area = function(point: WrapJsValue.point): number {
  return point.x * (point.y === undefined ? 1 : point.y);
};
