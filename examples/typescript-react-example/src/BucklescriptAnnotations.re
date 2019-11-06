[@genType]
type someMutableFields = {
  .
  [@bs.set] "mutable0": string,
  "immutable": int,
  [@bs.set] "mutable1": string,
  [@bs.set] "mutable2": string,
};

// [@genType]
// type someMethods = {
//   .
//   [@bs.meth] "send": string => unit,
//   [@bs.meth] "on": (string, (. int) => unit) => unit,
// };
