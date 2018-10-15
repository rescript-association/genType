/**
  * Wrap component MyBanner to be used from Reason.
  */
[@genType.import "./MyBanner.component"] /* Module with the JS component to be wrapped. */
[@bs.module "./MyBannerWrapper.re"] /* This must always be the name of the current module. */
/* The make function will be automatically generated from the types below. */
external make:
  (~show: bool, ~message: Js.Nullable.t(string), 'a) =>
  ReasonReact.component(
    ReasonReact.stateless,
    ReasonReact.noRetainedProps,
    ReasonReact.actionless,
  ) =
  "";

let make = make;