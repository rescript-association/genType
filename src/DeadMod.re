/***************************************************************************/
/*                                                                         */
/*   Copyright (c) 2014-2016 LexiFi SAS. All rights reserved.              */
/*                                                                         */
/*   This source code is licensed under the MIT License                    */
/*   found in the LICENSE file at the root of this source tree             */
/*                                                                         */
/***************************************************************************/

open Ident;
open Types;
open Typedtree;

open DeadCommon;

let rec getSignature = (~isfunc=false, moduleType) =>
  switch (moduleType) {
  | Mty_signature(signature) => signature
  | Mty_functor(_, tOpt, _) when isfunc =>
    switch (tOpt) {
    | None => []
    | Some(moduleType) => getSignature(moduleType)
    }
  | Mty_functor(_, _, moduleType) => getSignature(moduleType)
  | _ => []
  };

