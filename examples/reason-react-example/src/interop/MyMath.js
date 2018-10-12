/* @flow strict */

export const round: number => number = Math.round;

export const area = function(point: { x: number, y?: number }): number {
  return point.x * (point.y === undefined ? 1 : point.y);
};
