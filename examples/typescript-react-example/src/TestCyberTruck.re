let rec fox = x => box(x)
and box = x => fox(x);

let a = 3;

let b = a + 1;