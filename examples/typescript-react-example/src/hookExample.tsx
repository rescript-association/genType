import * as React from "react";

export const foo = (x: {
  person: { readonly name: string; readonly age: number };
}) => x.person.name;

type Props = {
  readonly person: { readonly name: string; readonly age: number };
  readonly children: JSX.Element;
  readonly renderMe: React.FC<{
    randomString: string;
    readonly poly: string;
  }>;
};

export const make: React.FC<Props> = (x: Props) => (
  <div>
    {" "}
    {x.person.name} {x.children}{" "}
    {x.renderMe({ randomString: "random-string", poly: "" })}{" "}
  </div>
);

class AsClassComponent extends React.PureComponent<Props> {
  public render() {
    return (
      <div>
        {" "}
        {this.props.person.name} {this.props.children}{" "}
        {this.props.renderMe({ randomString: "random-string", poly: "" })}{" "}
      </div>
    );
  }
}

export const makeRenamed = AsClassComponent;

export default make;
