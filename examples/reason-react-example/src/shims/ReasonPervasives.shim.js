/**
 * @flow strict
 * @nolint
 */

// $FlowExpectedError: Reason checked type sufficiently
export opaque type SafeReasonValueNotExportedToFlow = any;
'use strict';

// $FlowExpectedError: Reason checked type sufficiently
const $$Array = require('bs-platform/lib/js/array');

// $FlowExpectedError: Reason checked type sufficiently
export opaque type EmptyList = any;

// $FlowExpectedError: Reason checked type sufficiently
export opaque type Cons<t> = any;

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
