// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Belt_List from "rescript/lib/es6/belt_List.js";
import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Pervasives from "rescript/lib/es6/pervasives.js";
import * as Belt_Option from "rescript/lib/es6/belt_Option.js";
import * as Caml_option from "rescript/lib/es6/caml_option.js";

function computeArea(param) {
  return Math.imul(Math.imul(param.x, param.y), Belt_Option.mapWithDefault(param.z, 1, (function (n) {
                    return n;
                  })));
}

function coord2d(x, y) {
  return {
          x: x,
          y: y,
          z: undefined
        };
}

var getOpt = Belt_Option.mapWithDefault;

function findAddress(business) {
  return Belt_Option.mapWithDefault(business.address, /* [] */0, (function (a) {
                return {
                        hd: a,
                        tl: /* [] */0
                      };
              }));
}

function findAllAddresses(businesses) {
  return Belt_List.toArray(Belt_List.flatten(Belt_List.fromArray(Belt_Array.map(businesses, (function (business) {
                            return Pervasives.$at(Belt_Option.mapWithDefault(business.address, /* [] */0, (function (a) {
                                              return {
                                                      hd: a,
                                                      tl: /* [] */0
                                                    };
                                            })), Belt_Option.mapWithDefault(business.owner, /* [] */0, (function (p) {
                                              return Belt_Option.mapWithDefault(p.address, /* [] */0, (function (a) {
                                                            return {
                                                                    hd: a,
                                                                    tl: /* [] */0
                                                                  };
                                                          }));
                                            })));
                          })))));
}

function getPayload(param) {
  return param.payload;
}

function getPayloadRecord(param) {
  return param.payload;
}

var recordValue = {
  v: 1,
  w: 1
};

var payloadValue = {
  num: 1,
  payload: recordValue
};

function getPayloadRecordPlusOne(param) {
  var payload = param.payload;
  return {
          v: payload.v + 1 | 0,
          w: payload.w
        };
}

function findAddress2(business) {
  return Belt_Option.mapWithDefault(Caml_option.nullable_to_opt(business.address2), /* [] */0, (function (a) {
                return {
                        hd: a,
                        tl: /* [] */0
                      };
              }));
}

var someBusiness2_owner = null;

var someBusiness2_address2 = null;

var someBusiness2 = {
  name: "SomeBusiness",
  owner: someBusiness2_owner,
  address2: someBusiness2_address2
};

function computeArea3(o) {
  return Math.imul(Math.imul(o.x, o.y), Belt_Option.mapWithDefault(Caml_option.nullable_to_opt(o.z), 1, (function (n) {
                    return n;
                  })));
}

function computeArea4(o) {
  return Math.imul(Math.imul(o.x, o.y), Belt_Option.mapWithDefault(o.z, 1, (function (n) {
                    return n;
                  })));
}

function testMyRec(x) {
  return x.type_;
}

function testMyRec2(x) {
  return x;
}

function testMyObj(x) {
  return x.type_;
}

function testMyObj2(x) {
  return x;
}

function testMyRecBsAs(x) {
  return [
          x.jsValid0,
          x.type,
          x["the-key"],
          x["with\"dquote"],
          x["with'squote"],
          x["1number"]
        ];
}

function testMyRecBsAs2(x) {
  return x;
}

var origin = {
  x: 0,
  y: 0,
  z: 0
};

var someBusiness = {
  name: "SomeBusiness",
  owner: undefined,
  address: undefined
};

export {
  origin ,
  computeArea ,
  coord2d ,
  getOpt ,
  findAddress ,
  someBusiness ,
  findAllAddresses ,
  getPayload ,
  getPayloadRecord ,
  recordValue ,
  payloadValue ,
  getPayloadRecordPlusOne ,
  findAddress2 ,
  someBusiness2 ,
  computeArea3 ,
  computeArea4 ,
  testMyRec ,
  testMyRec2 ,
  testMyObj ,
  testMyObj2 ,
  testMyRecBsAs ,
  testMyRecBsAs2 ,
  
}
/* No side effect */
