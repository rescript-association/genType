type variant =
  | [@dead "variant.R"] R(int);

[@genType]
type record = {[@dead "record.variant"] variant: variant};

type r2 = {[@dead "r2.r2"] r2: int};

type r3 = {[@dead "r3.r3"] r3: int};

type r4 = {[@dead "r4.r4"] r4: int};

[@genType]
type annotatedVariant =
  | [@dead "annotatedVariant.R2"] R2(r2, r3)
  | [@dead "annotatedVariant.R4"] R4(r4);