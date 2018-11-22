/* @flow strict */

const Types = require("./Types.gen");

export type anInterestingFlowType = {
  an: string,
  interesting: number,
  type: string
};

export const c: anInterestingFlowType = Types.identity({
  an: "",
  interesting: 3,
  type: ""
});

export const MONDAY: weekday = "monday";
export const SATURDAY: weekday = "saturday";
export const SUNDAY: weekday = "sunday";
export type weekday = "monday" | "saturday" | "sunday";
