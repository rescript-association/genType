type r =
  | R(int);

[@genType]
type q = {w: r};

/*
 type q2 = {w2: int};

 [@genType]
 type r2 =
   | R2(q2);
   */