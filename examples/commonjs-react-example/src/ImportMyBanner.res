@ocaml.doc("
  * Wrap component MyBanner to be used from Reason.
  ")
@genType.import("./MyBanner.component")
external /* Module with the JS component to be wrapped. */
/* The make function will be automatically generated from the types below. */
make: (
  ~show: bool,
  @genType.as("Message") ~message: Js.Nullable.t<string>,
  'a,
) => ReasonReact.component<
  ReasonReact.stateless,
  ReasonReact.noRetainedProps,
  ReasonReact.actionless,
> = "make"

let make = make

