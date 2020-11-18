
module type MonadThing = {
  type t('a);
  let map: (t('a), ~f: 'a => 'b) => t('b);
  let bind: (t('a), ~f: 'a => t('b)) => t('b);
  let consume: (t('a), ~f: 'a => unit) => unit;
};

module Option = {
  type t('a) = option('a);
  let map = (value, ~f as use) =>
    switch (value) {
    | Some(x) => Some(use(x))
    | None => None
    };
  let bind = (value, ~f as use) =>
    switch (value) {
    | Some(x) => use(x)
    | None => None
    };
  let consume = (value, ~f as use) =>
    switch (value) {
    | Some(x) => use(x)
    | None => ()
    };
};

module O: MonadThing = Option;

module Result = {
  let map /*: t 'a 'b => f::('a => 'c) => t 'c 'b*/ = (value, ~f as use) =>
    switch (value) {
    | Ok(x) => Ok(use(x))
    | Error(e) => Error(e)
    };

  let bind: (result('a, 'b), ~f: 'a => result('c, 'b)) => result('c, 'b) =
    (value, ~f as use) =>
      switch (value) {
      | Ok(x) => use(x)
      | Error(e) => Error(e)
      };
  let consume: (result('a, 'b), ~f: 'a => unit) => unit =
    (value, ~f as use) =>
      switch (value) {
      | Ok(x) => use(x)
      | Error(_) => ()
      };
};
