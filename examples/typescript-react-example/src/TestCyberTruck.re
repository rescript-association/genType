let progress = () => ();

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

let a = 3;

let b = a + 1;