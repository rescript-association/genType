let rec fox = x => {
  let alias = fox;
  box(x);
}
and box = x => {
  x |> takeParseFunction(~parseFunction=fox);
  fox(x);
}
and takeParseFunction = (x, ~parseFunction) => parseFunction(x);

let a = 3;

let b = a + 1;