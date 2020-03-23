// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE

import * as React from "react";
import * as JSResource from "JSResource";
import * as Caml_option from "bs-platform/lib/es6/caml_option.js";
import * as DeadValueTest from "./DeadValueTest.bs.js";
import * as ImmutableArray from "./ImmutableArray.bs.js";
import * as BootloaderResource from "BootloaderResource";

console.log(ImmutableArray.fromArray);

var Inner = { };

var M = { };

var VariantUsedOnlyInImplementation = {
  a: /* A */0
};

var UnderscoreInside = { };

var MM = {
  x: 55,
  y: 55
};

console.log(55);

console.log(DeadValueTest.valueAlive);

function unusedRec(_param) {
  while(true) {
    _param = /* () */0;
    continue ;
  };
}

function split_map(l) {
  split_map(l);
  return /* [] */0;
}

function rec1(_param) {
  while(true) {
    _param = /* () */0;
    continue ;
  };
}

function rec2(_param) {
  while(true) {
    _param = /* () */0;
    continue ;
  };
}

function recWithCallback(_param) {
  while(true) {
    _param = /* () */0;
    continue ;
  };
}

function foo(_param) {
  while(true) {
    _param = /* () */0;
    continue ;
  };
}

function bar(param) {
  return foo(/* () */0);
}

function withDefaultValue(paramWithDefaultOpt, y) {
  var paramWithDefault = paramWithDefaultOpt !== undefined ? paramWithDefaultOpt : 3;
  return paramWithDefault + y | 0;
}

var Ext_buffer = { };

console.log(/* Root */["xzz"]);

var reasonResource = JSResource("DynamicallyLoadedComponent.bs");

function makeProps(prim, prim$1, prim$2) {
  var tmp = {
    s: prim
  };
  if (prim$1 !== undefined) {
    tmp.key = Caml_option.valFromOption(prim$1);
  }
  return tmp;
}

function make(props) {
  return React.createElement(BootloaderResource.read(reasonResource).make, props);
}

var LazyDynamicallyLoadedComponent = {
  reasonResource: reasonResource,
  makeProps: makeProps,
  make: make
};

var reasonResource$1 = JSResource("DynamicallyLoadedComponent.bs");

function makeProps$1(prim, prim$1, prim$2) {
  var tmp = {
    s: prim
  };
  if (prim$1 !== undefined) {
    tmp.key = Caml_option.valFromOption(prim$1);
  }
  return tmp;
}

function make$1(props) {
  return React.createElement(BootloaderResource.read(reasonResource$1).make, props);
}

var LazyDynamicallyLoadedComponent2 = {
  reasonResource: reasonResource$1,
  makeProps: makeProps$1,
  make: make$1
};

var cmp = React.createElement(make, {
      s: "hello"
    });

var cmp2 = React.createElement(make$1, {
      s: "hello"
    });

var fortytwo = 42;

var fortyTwoButExported = 42;

var thisIsUsedOnce = 34;

var thisIsUsedTwice = 34;

var thisIsKeptAlive = 42;

var thisIsMarkedLive = 42;

export {
  fortytwo ,
  fortyTwoButExported ,
  thisIsUsedOnce ,
  thisIsUsedTwice ,
  thisIsKeptAlive ,
  thisIsMarkedLive ,
  Inner ,
  M ,
  VariantUsedOnlyInImplementation ,
  UnderscoreInside ,
  MM ,
  unusedRec ,
  split_map ,
  rec1 ,
  rec2 ,
  recWithCallback ,
  foo ,
  bar ,
  withDefaultValue ,
  Ext_buffer ,
  LazyDynamicallyLoadedComponent ,
  LazyDynamicallyLoadedComponent2 ,
  cmp ,
  cmp2 ,
  
}
/*  Not a pure module */
