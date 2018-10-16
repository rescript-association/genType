/* @flow strict */

const Types = require("./Types.re");

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
