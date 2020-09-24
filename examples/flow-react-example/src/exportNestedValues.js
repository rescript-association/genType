/* @flow strict */

class InnerClass {
  static InnerStuff: {
    innerStuffContents: {| +x: number |},
    innerStuffContentsEmpty: {| |},
  } = {
    innerStuffContents: { x: 34 },
    innerStuffContentsEmpty: Object.freeze({}),
  };
}

export class TopLevelClass {
  static MiddleLevelElements: { stuff: typeof(InnerClass) } = {
    stuff: InnerClass,
  };
}

export const ValueStartingWithUpperCaseLetter =
  "ValueStartingWithUpperCaseLetter";

export default 42;
