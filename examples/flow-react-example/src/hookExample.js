/* @flow strict */

import * as React from "react";

export const foo = function(x: { +person: { +name: string, +age: number } }) {
  return x.person.name;
};

export const make = (x: {
  +person: { +name: string, +age: number },
  +children: React.Node
}) => <div> x.person.name children </div>;
