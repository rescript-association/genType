module Error2 = {
  type t = int;
  let notification = _ => ("", "");
};

module MyErrorHandler = ErrorHandler.Make(Error2);

//MyErrorHandler.notify(42);