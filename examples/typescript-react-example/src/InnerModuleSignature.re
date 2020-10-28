module InnerModule: {
  type t = pri string;
  [@genType]
  let make: t => string;
} = {
  type t = string;
  let make = t => t ++ "...";
};
