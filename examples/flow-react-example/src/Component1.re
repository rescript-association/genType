/* This is the basic component. */
let component = ReasonReact.statelessComponent("Page");

/* Your familiar handleClick from ReactJS. This mandatorily takes the payload,
   then the `self` record, which contains state (none here), `handle`, `reduce`
   and other utilities */
let handleClick = (_event, _self) => Js.log("clicked!");

/* `make` is the function that mandatorily takes `children` (if you want to use
   `JSX). `message` is a named argument, which simulates ReactJS props. Usage:

   `<Page message="hello" />`

   Which desugars to

   `ReasonReact.element(Page.make(~message="hello", [||]))` */

[@genType]
[@react.component]
let make = (~message="default message") => {
  <div> {ReasonReact.string(message)} </div>;
};

[@genType]
let plus = (x, _y) => x + 1;

[@genType]
let concat = (x, y) =>
  switch (y) {
  | None => None
  | Some(v) => Some(x ++ v)
  };

[@genType]
let consumeVariant = x =>
  switch (x) {
  | Component2.A => 1
  | B(n1, n2) => n1 + n2 + 2
  | C(n) =>
    (
      switch (n) {
      | None => 0
      | Some(v) => v
      }
    )
    + 3
  };

[@genType]
let l = [1, 2, 3];

[@genType]
let map = List.map;