/* @flow strict */

const pack = (x: "remove-range" | "normal", y: string, z: string) => {
  return x;
};

const packInt = (x: 0 | 34 | 35, y: string, z: string) => {
  return z;
};

exports.pack = pack;
exports.packInt = packInt;
