// A progress function will eventually terminate
let progress = {
  let counter = ref(Random.int(100));
  () => {
    if (counter^ < 0) {
      assert(false);
    };
    counter := counter^ - 1;
  };
};

// Another progress function
let progress2 = progress;

// A progress function can be taken from a module
module Progress = {
  module Nested = {
    let f = progress;
  };
};

// Need to declare at least one progress function and one recursive definition
[@progress progress]
let rec justReturn = () => ();

[@progress progress]
let rec alwaysLoop = () => alwaysLoop(); // infinite loop

[@progress progress]
let rec alwaysProgress = () => {
  // Terminates
  progress();
  alwaysProgress();
};

[@progress progress]
let rec alwaysProgressWrongOrder = () => {
  alwaysProgressWrongOrder();
  progress(); // Oops: this is too late
};

[@progress progress]
let rec doNotAlias = () => {
  // Must not alias recursive functions
  let alias = doNotAlias;
  alias();
};

[@progress (progress, progress2)]
// Terminates as each branch makes progress
let rec progressOnBothBranches = x => {
  if (x > 3) {
    progress();
  } else {
    progress2();
  };
  progressOnBothBranches(x);
};

[@progress progress]
// Loops as progress is only on one branch
let rec progressOnOneBranch = x => {
  if (x > 3) {
    progress();
  };
  progressOnOneBranch(x);
};

[@progress progress]
// callParseFunction is parametric: it takes a parse function and calls it
let rec testParametricFunction = x => {
  if (x > 3) {
    progress();
  };
  testParametricFunction2(x);
}
and testParametricFunction2 = x => {
  callParseFunction(x, ~parseFunction=testParametricFunction);
}
and callParseFunction = (x, ~parseFunction) => parseFunction(x); // loops

[@progress Progress.Nested.f]
let rec testCacheHit = x => {
  if (x > 0) {
    doNothing(x);
    doNothing(x); // this should hit the analysis cache
    Progress.Nested.f();
  };
  testCacheHit(x);
}
and doNothing = _ => ();

[@progress progress]
// Loops as can't rely on a specific evaluation order
let rec evalOrderIsNotLeftToRight = x => {
  let combineTwoUnits = ((), ()) => ();
  combineTwoUnits(progress(), evalOrderIsNotLeftToRight(x));
};

[@progress progress]
// Loops as can't rely on a specific evaluation order
let rec evalOrderIsNotRightToLeft = x => {
  let combineTwoUnits = ((), ()) => ();
  combineTwoUnits(evalOrderIsNotRightToLeft(x), progress());
};

[@progress progress]
// Terminates: all arguments are evaluated in some order
let rec butFirstArgumentIsAlwaysEvaluated = x => {
  let combineTwoUnits = ((), ()) => ();
  combineTwoUnits(progress(), ());
  butFirstArgumentIsAlwaysEvaluated(x);
};

[@progress progress]
// Terminates: all arguments are evaluated in some order
let rec butSecondArgumentIsAlwaysEvaluated = x => {
  let combineTwoUnits = ((), ()) => ();
  combineTwoUnits((), progress());
  butSecondArgumentIsAlwaysEvaluated(x);
};

module Parser = {
  type token =
    | Asterisk
    | Int(int)
    | Eof;

  type position = {
    lnum: int,
    cnum: int,
  };

  type t = {
    mutable position,
    mutable errors: list(string),
    mutable token,
  };

  let tokenToString = token =>
    switch (token) {
    | Asterisk => "*"
    | Eof => "Eof"
    | Int(n) => string_of_int(n)
    };

  let next = p => {
    p.token = Random.bool() ? Eof : Int(Random.int(1000));
    p.position = {lnum: Random.int(1000), cnum: Random.int(80)};
  };

  let err = (p, s) => p.errors = [s, ...p.errors];

  let expect = (p, token) =>
    if (p.token == token) {
      next(p);
    } else {
      err(p, "expected token " ++ tokenToString(p.token));
    };
};

[@progress Parser.next]
let rec parseListInt = p => parseList(p, ~f=parseInt)

[@progress]
and parseListListInt = p => parseList(p, ~f=parseListInt)

// Annotating parseList is an error: can't be analyzed directly as it takes a parameter f
[@progress]
and parseList: 'a. (Parser.t, ~f: Parser.t => 'a) => list('a) =
  (p: Parser.t, ~f) =>
    if (p.token == Asterisk) {
      [];
    } else {
      let item = f(p);
      let l = parseList(p, ~f);
      [item, ...l];
    }
and parseInt = (p: Parser.t) => {
  let res =
    switch (p.token) {
    | Int(n) => n
    | _ =>
      Parser.err(p, "integer expected");
      (-1);
    };
  Parser.next(p);
  res;
};