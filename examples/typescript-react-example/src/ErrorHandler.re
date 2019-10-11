module type Error = {
  type t;
  let notification: t => (string, string);
};

module Make = (Error: Error) => {
  let notify = x => Error.notification(x);
};