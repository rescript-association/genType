export type weekday = [
| #monday
| #tuesday
| #wednesday
| #thursday
| #friday
| #saturday
| #sunday
]

export isWeekend = (x: weekday) =>
  switch x {
  | #saturday | #sunday => true
  | _ => false
  }

export monday = #monday
export saturday = #saturday
export sunday = #sunday

export onlySunday = (_: [ | #sunday ]) => ()

export swap = x =>
  switch x {
  | #sunday => #saturday
  | #saturday => #sunday
  }

export type testGenTypeAs = [
| @genType.as("type")
#type_
| @genType.as("module")
#module_
| @genType.as("42")
#fortytwo
]

export testConvert = (x: testGenTypeAs) => x

export fortytwoOK: testGenTypeAs = #fortytwo

/* Exporting this is BAD: type inference means it's not mapped to "42" */
export fortytwoBAD = #fortytwo

export type testGenTypeAs2 = [
| @genType.as("type")
#type_
| @genType.as("module")
#module_
| @genType.as("42")
#fortytwo
]

/* Since testGenTypeAs2 is the same type as testGenTypeAs1,
 share the conversion map. */
export testConvert2 = (x: testGenTypeAs2) => x

export type testGenTypeAs3 = [
| @genType.as("type")
#type_
| @genType.as("module")
#module_
| @genType.as("XXX THIS IS DIFFERENT")
#fortytwo
]

/* Since testGenTypeAs3 has a different representation:
 use a new conversion map. */
export testConvert3 = (x: testGenTypeAs3) => x

/* This converts between testGenTypeAs2 and testGenTypeAs3 */
export testConvert2to3 = (x: testGenTypeAs2): testGenTypeAs3 => x

export type x1 = [ | #x | @genType.as("same") #x1 ]

export type x2 = [ | #x | @genType.as("same") #x2 ]

export id1 = (x: x1) => x

export id2 = (x: x2) => x

@genType.as("type")
export type type_ = | @genType.as("type") Type

export polyWithOpt = foo =>
  foo === "bar"
    ? None
    : switch foo !== "baz" {
      | true => Some(#One(foo))
      | false => Some(#Two(1))
      }

export type result1<'a, 'b> =
  | Ok('a)
  | Error('b)

export type result2<'a, 'b> = result<'a, 'b>

export type result3<'a, 'b> = Belt.Result.t<'a, 'b>

export restResult1 = (x: result1<int, string>) => x

export restResult2 = (x: result2<int, string>) => x

export restResult3 = (x: result3<int, string>) => x
