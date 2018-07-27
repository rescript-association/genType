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

[@genFlow]
let make = (~message="default message", _children) => {
  ...component,
  render: self =>
    <div onClick=(self.handle(handleClick))>
      (ReasonReact.string(message))
    </div>,
};

[@genFlow]
let plus = (x, _y) => x + 1;

[@genFlow]
let concat = (x, y) =>
  switch (y) {
  | None => None
  | Some(v) => Some(x ++ v)
  };

[@genFlow]
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

[@genFlow]
let l = [1, 2, 3];

[@genFlow]
let map = List.map;