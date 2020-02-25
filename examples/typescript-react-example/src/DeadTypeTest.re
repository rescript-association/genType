type t =
  | A
  | B;
let a = A;

type deadType =
  | OnlyInImplementation
  | OnlyInInterface
  | InBoth
  | InNeither;

let _ = OnlyInImplementation;
let _ = InBoth;