/**
 *
 * @flow strict
 * @format
 * @nolint
 */

export opaque type SafeReasonValueNotExportedToFlow = any;
'use strict';

const $$Array = require('bs-platform/lib/js/array');

export opaque type EmptyList = any;

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
