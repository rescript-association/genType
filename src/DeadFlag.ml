(***************************************************************************)
(*                                                                         *)
(*   Copyright (c) 2014-2016 LexiFi SAS. All rights reserved.              *)
(*                                                                         *)
(*   This source code is licensed under the MIT License                    *)
(*   found in the LICENSE file at the root of this source tree             *)
(*                                                                         *)
(***************************************************************************)

type threshold = {exceptions: int; percentage: float; optional: [`Percent | `Both]}


type opt = {call_sites: bool; threshold: threshold}

let opta = ref
  {
    call_sites = false;
    threshold =
      {
        exceptions = 0;
        percentage = 1.;
        optional = `Percent
      };
  }



type style = {opt_arg: bool; unit_pat: bool; seq: bool; binding: bool}
let style = ref
  {
    opt_arg = false;
    unit_pat = false;
    seq = false;
    binding = false;
  }

type basic = {call_sites: bool; threshold: int}
let exported : basic ref = ref
  ({
    call_sites = false;
    threshold = 0
  } : basic)

let typ : basic ref = ref
  ({
    call_sites = false;
    threshold = 0
  } : basic)

let verbose = ref false

let underscore = ref false

let internal = ref false
