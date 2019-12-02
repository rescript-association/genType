let progress = () => ();
let progress2 = () => ();

module Progress = {
  module Nested = {
    let f = () => ();
  };
};

module Loops = {
  [@progress (progress, progress2)]
  let rec fox = x => {
    let alias = fox;
    if (x != x) {
      progress();
    };
    box(x);
  }
  and box = x => {
    x |> takeParseFunction(~parseFunction=fox);
  }
  and takeParseFunction = (x, ~parseFunction) => parseFunction(x);
};

module Terminates = {
  [@progress progress]
  let rec fox2 = x => {
    if (x != x) {
      progress();
    } else {
      progress();
    };
    box2(x);
  }
  and box2 = x => {
    x |> takeParseFunction2(~parseFunction=fox2);
    fox2(x);
  }
  and takeParseFunction2 = (x, ~parseFunction) => parseFunction(x);

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
};

let a = 3;

let b = a + 1;