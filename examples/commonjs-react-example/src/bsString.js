/* @flow strict */

const pack: (
  "remove-range" | "normal",
  string,
  string
) => "remove-range" | "normal" = (x, y, z) => {
  return x;
};

const packInt: (0 | 34 | 35, string, string) => string = (x, y, z) => {
  return z;
};

exports.pack = pack;
exports.packInt = packInt;
