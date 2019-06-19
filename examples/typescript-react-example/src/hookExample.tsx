import * as React from "react";

export const foo = (x: {
  person: { readonly name: string; readonly age: number };
}) => x.person.name;

type Props = {
  readonly person: { readonly name: string; readonly age: number };
  readonly children: JSX.Element;
};

export const make: React.FC<Props> = (x: Props) => (
  <div>
    {" "}
    {x.person.name} {x.children}{" "}
  </div>
);

export default make;
