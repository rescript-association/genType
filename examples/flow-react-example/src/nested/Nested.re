[@genType]
type variant =
  | A
  | B(int, int)
  | C(option(int));

[@genType]
let consumeVariant = x =>
  switch (x) {
  | Component2.A => 1
  | B(n1, n2) => n1 + n2 + 2
  | C(n) =>
    (
      switch (n) {
      | None => 0
      | Some(v) => v
      }
    )
    + 3
  };