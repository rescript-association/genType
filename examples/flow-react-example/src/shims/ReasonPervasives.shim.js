/**
 * @flow strict
 * @nolint
 */

export opaque type SafeReasonValueNotExportedToFlow = mixed;
'use strict';

// $FlowExpectedError: Reason checked type sufficiently
const $$Array = require('bs-platform/lib/js/array');

export opaque type EmptyList = mixed;

export opaque type Cons<t> = mixed;

export type list<t> = | Cons<t> | EmptyList;

function cons(itm, lst) {
  return /* :: */[
          itm,
          lst
        ];
}

const emptyList = /* [] */0;

const fromArray = $$Array.to_list;

exports.emptyList = (emptyList : EmptyList);
exports.cons = (cons : <t>(t, list<t>)=>list<t>);
exports.fromArray = (fromArray : <t>(Array<t>)=>list<t>);
