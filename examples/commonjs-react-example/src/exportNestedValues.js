/* @flow strict */

class InnerClass {
  static InnerStuff: { innerStuffContents: {| x: number |} } = {
    innerStuffContents: { x: 34 },
  };
}

export class TopLevelClass {
  static MiddleLevelElements: { stuff: typeof InnerClass } = {
    stuff: InnerClass,
  };
}

export const ValueStartingWithUpperCaseLetter =
  "ValueStartingWithUpperCaseLetter";

export default 42;
