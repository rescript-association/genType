/* @flow strict */

import * as React from "react";

export const foo = function(x: { +person: { +name: string, +age: number } }) {
  return x.person.name;
};

type Props = {|
  +person: { +name: string, +age: number },
  +children: React.Node,
  +renderMe: React.ComponentType<{|
    +randomString: string,
    +poly: string
  |}>
|};

export class make extends React.Component<Props> {
  render() {
    const RenderMe = this.props.renderMe;
    return (
      <div>
        {" "}
        {this.props.person.name} {this.props.children}{" "}
        <RenderMe randomString="random-string" poly="" />
      </div>
    );
  }
}

export const makeRenamed = make;

export default make;
