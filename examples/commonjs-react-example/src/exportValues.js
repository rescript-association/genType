/* @flow strict */

const exportNestedValues = 42;

class InnerClass {
  static InnerStuff = {
    innerStuffContents: { x: 34 }
  };
}

class TopLevelClass {
  static MiddleLevelElements = {
    stuff: InnerClass
  };
}

module.exports = exportNestedValues;
exports.InnerClass = InnerClass;

export type someType = { x: number };
