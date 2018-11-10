module M = {
  module Inner = {
    type t = string;
  };
};

module type M = {};

[@genType]
type a = M.Inner.t;