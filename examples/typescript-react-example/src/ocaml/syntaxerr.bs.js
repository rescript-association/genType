// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE

import * as Block from "bs-platform/lib/es6/block.js";
import * as Curry from "bs-platform/lib/es6/curry.js";
import * as Printf from "bs-platform/lib/es6/printf.js";
import * as $$Location from "./location.bs.js";
import * as Caml_exceptions from "bs-platform/lib/es6/caml_exceptions.js";

var $$Error = Caml_exceptions.create("Syntaxerr.Error");

var Escape_error = Caml_exceptions.create("Syntaxerr.Escape_error");

function prepare_error(param) {
  switch (param.tag | 0) {
    case /* Unclosed */0 :
        var closing = param[3];
        var opening = param[1];
        return Curry._1($$Location.errorf(param[2], /* :: */[
                        Curry._1($$Location.errorf(param[0], undefined, undefined, /* Format */[
                                  /* String_literal */Block.__(11, [
                                      "This '",
                                      /* String */Block.__(2, [
                                          /* No_padding */0,
                                          /* String_literal */Block.__(11, [
                                              "' might be unmatched",
                                              /* End_of_format */0
                                            ])
                                        ])
                                    ]),
                                  "This '%s' might be unmatched"
                                ]), opening),
                        /* [] */0
                      ], Curry._2(Printf.sprintf(/* Format */[
                                /* String_literal */Block.__(11, [
                                    "Syntax error: '",
                                    /* String */Block.__(2, [
                                        /* No_padding */0,
                                        /* String_literal */Block.__(11, [
                                            "' expected, the highlighted '",
                                            /* String */Block.__(2, [
                                                /* No_padding */0,
                                                /* String_literal */Block.__(11, [
                                                    "' might be unmatched",
                                                    /* End_of_format */0
                                                  ])
                                              ])
                                          ])
                                      ])
                                  ]),
                                "Syntax error: '%s' expected, the highlighted '%s' might be unmatched"
                              ]), closing, opening), /* Format */[
                        /* String_literal */Block.__(11, [
                            "Syntax error: '",
                            /* String */Block.__(2, [
                                /* No_padding */0,
                                /* String_literal */Block.__(11, [
                                    "' expected",
                                    /* End_of_format */0
                                  ])
                              ])
                          ]),
                        "Syntax error: '%s' expected"
                      ]), closing);
    case /* Expecting */1 :
        return Curry._1($$Location.errorf(param[0], undefined, undefined, /* Format */[
                        /* String_literal */Block.__(11, [
                            "Syntax error: ",
                            /* String */Block.__(2, [
                                /* No_padding */0,
                                /* String_literal */Block.__(11, [
                                    " expected.",
                                    /* End_of_format */0
                                  ])
                              ])
                          ]),
                        "Syntax error: %s expected."
                      ]), param[1]);
    case /* Not_expecting */2 :
        return Curry._1($$Location.errorf(param[0], undefined, undefined, /* Format */[
                        /* String_literal */Block.__(11, [
                            "Syntax error: ",
                            /* String */Block.__(2, [
                                /* No_padding */0,
                                /* String_literal */Block.__(11, [
                                    " not expected.",
                                    /* End_of_format */0
                                  ])
                              ])
                          ]),
                        "Syntax error: %s not expected."
                      ]), param[1]);
    case /* Applicative_path */3 :
        return $$Location.errorf(param[0], undefined, undefined, /* Format */[
                    /* String_literal */Block.__(11, [
                        "Syntax error: applicative paths of the form F(X).t are not supported when the option -no-app-func is set.",
                        /* End_of_format */0
                      ]),
                    "Syntax error: applicative paths of the form F(X).t are not supported when the option -no-app-func is set."
                  ]);
    case /* Variable_in_scope */4 :
        var $$var = param[1];
        return Curry._2($$Location.errorf(param[0], undefined, undefined, /* Format */[
                        /* String_literal */Block.__(11, [
                            "In this scoped type, variable '",
                            /* String */Block.__(2, [
                                /* No_padding */0,
                                /* String_literal */Block.__(11, [
                                    " is reserved for the local type ",
                                    /* String */Block.__(2, [
                                        /* No_padding */0,
                                        /* Char_literal */Block.__(12, [
                                            /* "." */46,
                                            /* End_of_format */0
                                          ])
                                      ])
                                  ])
                              ])
                          ]),
                        "In this scoped type, variable '%s is reserved for the local type %s."
                      ]), $$var, $$var);
    case /* Other */5 :
        return $$Location.errorf(param[0], undefined, undefined, /* Format */[
                    /* String_literal */Block.__(11, [
                        "Syntax error",
                        /* End_of_format */0
                      ]),
                    "Syntax error"
                  ]);
    case /* Ill_formed_ast */6 :
        return Curry._1($$Location.errorf(param[0], undefined, undefined, /* Format */[
                        /* String_literal */Block.__(11, [
                            "broken invariant in parsetree: ",
                            /* String */Block.__(2, [
                                /* No_padding */0,
                                /* End_of_format */0
                              ])
                          ]),
                        "broken invariant in parsetree: %s"
                      ]), param[1]);
    case /* Invalid_package_type */7 :
        return Curry._1($$Location.errorf(param[0], undefined, undefined, /* Format */[
                        /* String_literal */Block.__(11, [
                            "invalid package type: ",
                            /* String */Block.__(2, [
                                /* No_padding */0,
                                /* End_of_format */0
                              ])
                          ]),
                        "invalid package type: %s"
                      ]), param[1]);
    
  }
}

$$Location.register_error_of_exn((function (param) {
        if (param[0] === $$Error) {
          return prepare_error(param[1]);
        }
        
      }));

function report_error(ppf, err) {
  return $$Location.report_error(ppf, prepare_error(err));
}

function location_of_error(param) {
  return param[0];
}

function ill_formed_ast(loc, s) {
  throw [
        $$Error,
        /* Ill_formed_ast */Block.__(6, [
            loc,
            s
          ])
      ];
}

export {
  $$Error ,
  Escape_error ,
  report_error ,
  location_of_error ,
  ill_formed_ast ,
  
}
/*  Not a pure module */
