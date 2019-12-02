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
  x |> callParseFunction(~parseFunction=testParametricFunction);
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