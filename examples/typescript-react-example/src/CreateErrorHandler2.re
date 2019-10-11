module Error2 = {
  type t = int;
  let notification = s => ("", "");
};

module MyErrorHandler = ErrorHandler.Make(Error2);

//MyErrorHandler.notify(42);