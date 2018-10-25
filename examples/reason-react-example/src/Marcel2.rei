module type ModType = {type t = string;};

module M: ModType;

[@genType]
type m = M.t;