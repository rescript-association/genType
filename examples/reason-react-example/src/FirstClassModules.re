module type MT = {
  let x: int;
  let y: string;
};
module M = {
  let y = "abc";
  let x = 42;
};

[@genType]
let firstClassModule: module MT = (module M);
