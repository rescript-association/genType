module Wrapper2 = {
  module Wrapper = {
    module type ModType = {type t = string;};
  };
};

module A = {
  type t = string;
};

module M3 = {
  module M2 = {
    module M1 = (val (module A): Wrapper2.Wrapper.ModType);
  };
};

[@genType]
type m = M3.M2.M1.t;