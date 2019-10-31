[@genType.opaque]
type opaqueFromRecords =
  | [@dead "opaqueFromRecords.A"] A(Records.coord);

[@genType]
let noConversion = (x: opaqueFromRecords) => x;

[@genType]
type pair = (opaqueFromRecords, opaqueFromRecords);

[@genType]
let testConvertNestedRecordFromOtherFile = (x: Records.business) => x;