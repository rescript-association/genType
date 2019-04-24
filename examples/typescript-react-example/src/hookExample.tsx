import * as React from "react";

export const foo = (x: {
  person: { readonly name: string; readonly age: number };
}) => x.person.name;

export const make = (x: {
  readonly person: { readonly name: string; readonly age: number };
  readonly children: JSX.Element;
}) => <div> {x.person.name} {x.children} </div>;
