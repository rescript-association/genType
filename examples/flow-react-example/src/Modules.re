module type T = {type t = int;};

module M: T = {
  type t = int;
};

[@genType]
type t = M.t => int;

module type T2 = {type t;};

module M2: T2 with type t = int = {
  type t = int;
};

[@genType]
type t2 = M2.t => int;

module type MT = {let foo: int => int;};

[@genType]
type firstClassModule = (module MT);