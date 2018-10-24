module type AConfig = {type configType;};

module A = {
  module Make = (Conf: AConfig) => {
    type a = Conf.configType;
    module Inner = {
      type inner = string;
    };
  };
};

module B =
  A.Make({
    type configType = int;
  });

module C = {
  type a = B.a;
  module Inner = {
    type inner = string;
  };
};

[@genType]
type c = C.a;

[@genType]
type d = C.Inner.inner;

[@genType]
type e = B.Inner.inner;