import * as React from "react";

export const foo = (x: {
  person: { readonly name: string; readonly age: number };
}) => x.person.name;

type Props = {
  readonly person: { readonly name: string; readonly age: number };
  readonly children: JSX.Element;
  readonly renderMe: React.FC<{ randomString: string; readonly poly: string }>;
};

export const make: React.FC<Props> = (x: Props) => (
  <div>
    {" "}
    {x.person.name} {x.children}{" "}
  </div>
);

export const makeRenamed = make;

export default make;
