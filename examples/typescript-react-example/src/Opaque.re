[@genType.opaque]
type opaqueFromRecords =
  | A(Records.coord);

[@genType]
let noConversion = (x: opaqueFromRecords) => x;