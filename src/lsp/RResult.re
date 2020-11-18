let resultOfOption = (err, v) =>
  switch (v) {
  | Some(v) => Ok(v)
  | None => Error(err)
  };
let orError = resultOfOption;

let toOptionAndLog = err =>
  switch (err) {
  | Error(e) =>
    Log.log(e);
    None;
  | Ok(v) => Some(v)
  };

module InfixResult = {
  let (|?>) = (a, fn) =>
    switch (a) {
    | Ok(a) => fn(a)
    | Error(e) => Error(e)
    };
  let (|?>>) = (a, fn) =>
    switch (a) {
    | Ok(a) => Ok(fn(a))
    | Error(e) => Error(e)
    };
  let (|?) = (a, default) =>
    switch (a) {
    | Ok(a) => a
    | Error(_) => default
    };
};
open InfixResult;
let withDefault = (d, v) => v |? d;
