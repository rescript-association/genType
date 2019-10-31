[@genType]
let testImmutableArrayGet = arr => ImmutableArray.(arr[3]);

/*
   type error
   let testImmutableArraySet = arr => ImmutableArray.(arr[3] = 4);
 */

[@dead "testBeltArrayGet"] let testBeltArrayGet = arr => Belt.(arr[3]);

[@dead "testBeltArraySet"] let testBeltArraySet = arr => Belt.(arr[3] = 4);