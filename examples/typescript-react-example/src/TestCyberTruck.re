let progress = () => ();

module Loops = {
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

  let u = ();

  let rec aa = x => {
    if (x > 0) {
      cc(x);
    } else {
      cc(x);
    };
    bb(x);
  }
  and bb = x => aa(x)
  and cc = _ => u;
};

let a = 3;

let b = a + 1;