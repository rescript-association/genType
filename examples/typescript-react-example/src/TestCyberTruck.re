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
    fox(x);
  }
  and takeParseFunction = (x, ~parseFunction) => parseFunction(x);
};

module Terminates = {
  let rec fox = x => {
    if (x != x) {
      progress();
    } else {
        progress();
    };
    box(x);
  }
  and box = x => {
    x |> takeParseFunction(~parseFunction=fox);
    fox(x);
  }
  and takeParseFunction = (x, ~parseFunction) => parseFunction(x);
};

let a = 3;

let b = a + 1;