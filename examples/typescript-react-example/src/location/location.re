/* Test file starting with lower case letter. */

[@genType]
type t = {
  [@dead "t.id"] id: string,
  [@dead "t.name"] name: string,
};

[@dead "x"] let x = 42;