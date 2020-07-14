@genType.opaque
type opaqueFromRecords = A(Records.coord)

export noConversion = (x: opaqueFromRecords) => x

export type pair = (opaqueFromRecords, opaqueFromRecords)

export testConvertNestedRecordFromOtherFile = (x: Records.business) => x
