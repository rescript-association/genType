/* The annotations are in the .rei file. */

module type ModType = {type t = string;};

module A = {
  type t = string;
};

module M = (val (module A): ModType);

type m = M.t;