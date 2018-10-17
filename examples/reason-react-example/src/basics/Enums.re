[@genType]
type weekday = [
  | `monday
  | `tuesday
  | `wednesday
  | `thursday
  | `friday
  | `saturday
  | `sunday
];

[@genType]
let isWeekend = (x: weekday) =>
  switch (x) {
  | `saturday
  | `sunday => true
  | _ => false
  };

[@genType]
let monday = `monday;
[@genType]
let saturday = `saturday;
[@genType]
let sunday = `sunday;

[@genType]
let onlySunday = (_: [ | `sunday]) => ();

[@genType]
let swap = x =>
  switch (x) {
  | `sunday => `saturday
  | `saturday => `sunday
  };

[@genType]
type testGenTypeAs = [
  | [@genType.as "type"] `type_
  | [@genType.as "module"] `module_
  | [@genType.as "42"] `fortytwo
];

[@genType]
let testConvert = (x: testGenTypeAs) => x;

[@genType]
let fortytwoOK: testGenTypeAs = `fortytwo;

/* Exporting this is BAD: type inference misses the mapping to "42" */
[@genType]
let fortytwoBAD = `fortytwo;

[@genType]
type testGenTypeAs2 = [
  | [@genType.as "type"] `type_
  | [@genType.as "module"] `module_
  | [@genType.as "42"] `fortytwo
];

[@genType]
let testConvert2 = (x: testGenTypeAs2) => x;

[@genType]
type testGenTypeAs3 = [
  | [@genType.as "type"] `type_
  | [@genType.as "module"] `module_
  | [@genType.as "XXX THIS IS DIFFERENT"] `fortytwo
];

[@genType]
let testConvert3 = (x: testGenTypeAs3) => x;