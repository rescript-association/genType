[@genType]
let testImmutableArrayGet = arr => ImmutableArray.(arr[3]);

/*
   type error
   let testImmutableArraySet = arr => ImmutableArray.(arr[3] = 4);
 */

let testBeltArrayGet = arr => Belt.(arr[3]);

let testBeltArraySet = arr => Belt.(arr[3] = 4);