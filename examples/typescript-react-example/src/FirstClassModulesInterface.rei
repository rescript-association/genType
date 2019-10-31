[@genType]
type record = {
  [@dead "record.x"] x: int,
  [@dead "record.y"] y: string,
};

[@dead "r"] let r: record;

[@genType]
module type MT = {let x: int;};

[@genType]
type firstClassModule = (module MT);