// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var TestBsIntGen = require("./TestBsInt.gen");

function packInt(prim, prim$1, prim$2) {
  return TestBsIntGen.packInt((function () {
                  switch (prim) {
                    case "a" :
                        return 0;
                    case "removeRange" :
                        return 34;
                    case "normal" :
                        return 35;
                    
                  }
                })(), prim$1, prim$2);
}

var x = TestBsIntGen.packInt(35, "a", "b");

exports.packInt = packInt;
exports.x = x;
/* x Not a pure module */
