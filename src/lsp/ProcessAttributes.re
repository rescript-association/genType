open SharedTypes;
open Infix;

/* TODO should I hang on to location? */
let rec findDocAttribute = attributes => {
  Parsetree.(
    switch (attributes) {
    | [] => None
    | [
        (
          {Asttypes.txt: "ocaml.doc"},
          PStr([
            {
              pstr_desc:
                Pstr_eval(
                  {pexp_desc: Pexp_constant(Pconst_string(doc, _))},
                  _,
                ),
            },
          ]),
        ),
        ..._,
      ] =>
      Some(PrepareUtils.cleanOffStars(doc))
    | [_, ...rest] => findDocAttribute(rest)
    }
  );
};

let newDeclared =
    (
      ~item,
      ~scope,
      ~extent,
      ~name,
      ~stamp,
      ~modulePath,
      ~processDoc,
      exported,
      attributes,
    ) => {
  {
    name,
    stamp,
    extentLoc: extent,
    scopeLoc: scope,
    exported,
    modulePath,
    docstring: findDocAttribute(attributes) |?>> processDoc,
    item,
    /* scopeType: Let, */
    /* scopeStart: env.scopeStart, */
  };
};
