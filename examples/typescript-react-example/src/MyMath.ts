/* @flow strict */

export const round: ((_: number) => number) = Math.round;

// tslint:disable-next-line:only-arrow-functions
export const area = function(point: { x: number; y?: number }): number {
  return point.x * (point.y === undefined ? 1 : point.y);
};
