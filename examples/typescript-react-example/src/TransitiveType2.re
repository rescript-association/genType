[@genType]
type t2 = option(TransitiveType3.t3);

[@genType]
let convertT2 = (x: t2) => x;