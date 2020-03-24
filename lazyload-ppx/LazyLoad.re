open Ast_helper;
open Ast_mapper;
open Asttypes;
open Parsetree;
open Longident;

let hasMappedStructure = ref(false);

module Resource = {
  type t = {
    name: string,
    loc: Location.t,
  };
  let compare = ({name: n1}, {name: n2}) => compare(n1, n2);
};

module ResourceSet = Set.Make(Resource);
module SM = Map.Make(String);

let jsResources = ref(ResourceSet.empty);
let topLevelExprs = ref(SM.empty);

let addTopLevelExpr = (bindingName, expr) => {
  let count = ref(0);
  let makeBindingName = (bindingName, count) =>
    count == 0 ? bindingName : bindingName ++ "$" ++ string_of_int(count);
  while (SM.mem(makeBindingName(bindingName, count^), topLevelExprs^)) {
    count := count^ + 1;
  };
  let bindingName = makeBindingName(bindingName, count^);
  let _ = topLevelExprs := SM.add(bindingName, expr, topLevelExprs^);
  bindingName;
};

let depIgnore = [
  (
    {loc: default_loc^, txt: "warning"},
    PStr([Str.eval(Exp.constant(Pconst_string("-3", None)))]),
  ),
];

let localModulePrefix = "$Local$";

let genLocal = ({Resource.name, loc}) =>
  with_default_loc(loc, () =>
    Str.modtype(
      Mtd.mk(
        ~typ=Mty.typeof_(Mod.ident({loc: default_loc^, txt: Lident(name)})),
        {loc: default_loc^, txt: localModulePrefix ++ name},
      ),
    )
  );

let genTopLevelBinding = ((txt, exp)) =>
  Vb.mk(Pat.var({loc: default_loc^, txt}), exp);

/* identical to signature. keep in sync. */
let structure = (mapper, structure) =>
  if (hasMappedStructure^) {
    default_mapper.structure(mapper, structure);
  } else {
    let _ = hasMappedStructure := true;
    let (fileAttributes, restOfStructure) =
      List.partition(
        str =>
          switch (str) {
          | {pstr_desc: Pstr_attribute(({txt: "bs.config"}, _))} => true
          | {pstr_desc: Pstr_extension(({txt: "bs.config"}, _), _)} => true
          | _ => false
          },
        structure,
      );
    let newStructure = default_mapper.structure(mapper, restOfStructure);
    fileAttributes
    @ List.map(genLocal, ResourceSet.elements(jsResources^))
    @ (
      switch (SM.bindings(topLevelExprs^)) {
      | [] => []
      | exprs => [
          Str.value(Nonrecursive, List.map(genTopLevelBinding, exprs)),
        ]
      }
    )
    @ newStructure;
  };

/* TODO */
let expr = (mapper, expr) =>
  switch (expr) {
  | {
      pexp_desc:
        Pexp_extension((
          {txt: "reasonResource", loc},
          PStr([
            {
              pstr_desc:
                Pstr_eval({pexp_desc: Pexp_construct({txt}, _)}, _),
            },
          ]),
        )),
    } as pexp =>
    let name = txt |> Longident.flatten |> List.hd;
    let _ =
      jsResources := ResourceSet.add({Resource.name, loc}, jsResources^);
    {
      ...pexp,
      pexp_desc:
        Pexp_constraint(
          Exp.apply(
            Exp.ident({
              loc: default_loc^,
              txt: Ldot(Lident("JSResource"), "jSResource"),
            }),
            [(Nolabel, Exp.constant(Pconst_string(name ++ ".bs", None)))],
          ),
          Typ.constr(
            {loc: default_loc^, txt: Ldot(Lident("JSResource"), "t")},
            [
              Typ.package(
                {loc: default_loc^, txt: Lident(localModulePrefix ++ name)},
                [],
              ),
            ],
          ),
        ),
    };
  | {
      pexp_desc:
        Pexp_extension((
          {txt: "requireDeferred", loc},
          PStr([
            {
              pstr_desc:
                Pstr_eval({pexp_desc: Pexp_construct({txt}, _)}, _),
            },
          ]),
        )),
    } =>
    let name = txt |> Longident.flatten |> List.hd;
    let _ =
      jsResources := ResourceSet.add({Resource.name, loc}, jsResources^);
    let bindingName = "$" ++ name ++ "$Deferred";
    let actualExp =
      Exp.constraint_(
        Exp.apply(
          Exp.ident({
            loc: default_loc^,
            txt: Ldot(Lident("RequireDeferred"), "make"),
          }),
          [(Nolabel, Exp.constant(Pconst_string(name ++ ".bs", None)))],
        ),
        Typ.constr(
          {loc: default_loc^, txt: Ldot(Lident("RequireDeferred"), "t")},
          [
            Typ.package(
              {loc: default_loc^, txt: Lident(localModulePrefix ++ name)},
              [],
            ),
          ],
        ),
      );
    let bindingName = addTopLevelExpr(bindingName, actualExp);
    Exp.ident({loc: default_loc^, txt: Lident(bindingName)});
  | {
      pexp_desc:
        Pexp_extension((
          {txt: "requireCond"},
          PStr([
            {
              pstr_desc:
                Pstr_eval(
                  {
                    pexp_desc:
                      Pexp_tuple([
                        conditionType,
                        condition,
                        {
                          pexp_desc:
                            Pexp_extension((
                              {txt: "bs.obj"},
                              PStr([
                                {
                                  pstr_desc:
                                    Pstr_eval(
                                      {
                                        pexp_desc:
                                          Pexp_record(
                                            [
                                              (
                                                {
                                                  txt:
                                                    Longident.Lident("true"),
                                                },
                                                {
                                                  pexp_desc:
                                                    Pexp_construct(
                                                      {
                                                        txt: thenTxt,
                                                        loc: thenLoc,
                                                      },
                                                      _,
                                                    ),
                                                  pexp_loc: pexp_loc_then,
                                                },
                                              ),
                                              (
                                                {
                                                  txt:
                                                    Longident.Lident("false"),
                                                },
                                                {
                                                  pexp_desc:
                                                    Pexp_construct(
                                                      {
                                                        txt: elseTxt,
                                                        loc: elseLoc,
                                                      },
                                                      _,
                                                    ),
                                                  pexp_loc: pexp_loc_else,
                                                },
                                              ),
                                            ] |
                                            [
                                              (
                                                {
                                                  txt:
                                                    Longident.Lident("false"),
                                                },
                                                {
                                                  pexp_desc:
                                                    Pexp_construct(
                                                      {
                                                        txt: elseTxt,
                                                        loc: elseLoc,
                                                      },
                                                      _,
                                                    ),
                                                  pexp_loc: pexp_loc_else,
                                                },
                                              ),
                                              (
                                                {
                                                  txt:
                                                    Longident.Lident("true"),
                                                },
                                                {
                                                  pexp_desc:
                                                    Pexp_construct(
                                                      {
                                                        txt: thenTxt,
                                                        loc: thenLoc,
                                                      },
                                                      _,
                                                    ),
                                                  pexp_loc: pexp_loc_then,
                                                },
                                              ),
                                            ],
                                            _,
                                          ),
                                      },
                                      [],
                                    ),
                                },
                              ]),
                            )),
                        },
                      ]),
                  },
                  _,
                ),
            },
          ]),
        )),
    } =>
    let thenModule = thenTxt |> Longident.flatten |> List.hd;
    let elseModule = elseTxt |> Longident.flatten |> List.hd;

    jsResources :=
      ResourceSet.add(
        {Resource.name: thenModule, loc: thenLoc},
        jsResources^,
      );
    jsResources :=
      ResourceSet.add(
        {Resource.name: elseModule, loc: elseLoc},
        jsResources^,
      );
    let bindingName =
      "$" ++ thenModule ++ "$OR$" ++ elseModule ++ "$RequireCond";

    let actualExp =
      Exp.constraint_(
        ~loc=pexp_loc_then,
        Exp.constraint_(
          ~loc=pexp_loc_else,
          Exp.apply(
            Exp.ident(
              ~attrs=depIgnore,
              {
                loc: default_loc^,
                txt: Ldot(Lident("RequireCond"), "either"),
              },
            ),
            [
              (Nolabel, conditionType),
              (Nolabel, condition),
              (
                Nolabel,
                Exp.extension((
                  Location.mkloc("bs.obj", default_loc^),
                  Parsetree.PStr([
                    Str.eval(
                      Exp.record(
                        [
                          (
                            Location.mkloc(
                              Longident.Lident("true"),
                              default_loc^,
                            ),
                            Exp.constant(
                              Pconst_string(thenModule ++ ".bs", None),
                            ),
                          ),
                          (
                            Location.mkloc(
                              Longident.Lident("false"),
                              default_loc^,
                            ),
                            Exp.constant(
                              Pconst_string(elseModule ++ ".bs", None),
                            ),
                          ),
                        ],
                        None,
                      ),
                    ),
                  ]),
                )),
              ),
            ],
          ),
          Typ.package(
            {
              loc: default_loc^,
              txt: Lident(localModulePrefix ++ elseModule),
            },
            [],
          ),
        ),
        Typ.package(
          {loc: default_loc^, txt: Lident(localModulePrefix ++ thenModule)},
          [],
        ),
      );
    let bindingName = addTopLevelExpr(bindingName, actualExp);
    Exp.ident({loc: default_loc^, txt: Lident(bindingName)});
  | {
      pexp_desc:
        Pexp_extension((
          {txt: "requireCond"},
          PStr([
            {
              pstr_desc:
                Pstr_eval(
                  {
                    pexp_desc:
                      Pexp_tuple([
                        conditionType,
                        condition,
                        {pexp_desc: Pexp_construct({txt, loc}, _)},
                      ]),
                  },
                  _,
                ),
            },
          ]),
        )),
    } =>
    let name = txt |> Longident.flatten |> List.hd;
    let _ =
      jsResources := ResourceSet.add({Resource.name, loc}, jsResources^);
    let bindingName = "$" ++ name ++ "$RequireCond";
    let actualExp =
      Exp.constraint_(
        Exp.apply(
          Exp.ident(
            ~attrs=depIgnore,
            {loc: default_loc^, txt: Ldot(Lident("RequireCond"), "make")},
          ),
          [
            (Nolabel, conditionType),
            (Nolabel, condition),
            (Nolabel, Exp.constant(Pconst_string(name ++ ".bs", None))),
          ],
        ),
        Typ.constr(
          {
            loc: default_loc^,
            txt: Ldot(Ldot(Lident("Js"), "Nullable"), "t"),
          },
          [
            Typ.package(
              {loc: default_loc^, txt: Lident(localModulePrefix ++ name)},
              [],
            ),
          ],
        ),
      );
    let bindingName = addTopLevelExpr(bindingName, actualExp);
    Exp.ident({loc: default_loc^, txt: Lident(bindingName)});
  | _ => default_mapper.expr(mapper, expr)
  };

/*
 module <Foo> = {
   let makeProps = <Foo>.makeProps;
   let make =
     React.createElement(
       props => {
         module Comp = (val JSResource.read([%reasonResource <Foo>]));
         Comp.make(props);
       },
       _,
     );
 };
 */
let module_expr = (mapper, module_expr) =>
  switch (module_expr) {
  | {
      pmod_desc:
        Pmod_extension((
          {txt: "lazyLoadComponent"},
          PStr([
            {
              pstr_desc:
                Pstr_eval({pexp_desc: Pexp_construct({txt}, _)}, _),
            },
          ]),
        )),
      pmod_loc,
    } as pmod =>
    with_default_loc(pmod_loc, () =>
      {
        ...pmod,
        pmod_desc:
          Pmod_structure([
            Str.value(
              Nonrecursive,
              [
                Vb.mk(
                  Pat.var({loc: default_loc^, txt: "reasonResource"}),
                  expr(
                    mapper,
                    Exp.extension((
                      {loc: default_loc^, txt: "reasonResource"},
                      PStr([
                        Str.eval(
                          Exp.construct({loc: default_loc^, txt}, None),
                        ),
                      ]),
                    )),
                  ),
                ),
              ],
            ),
            Str.value(
              Nonrecursive,
              [
                Vb.mk(
                  Pat.var({loc: default_loc^, txt: "makeProps"}),
                  Exp.ident({
                    loc: default_loc^,
                    txt: Ldot(txt, "makeProps"),
                  }),
                ),
              ],
            ),
            Str.value(
              Nonrecursive,
              [
                Vb.mk(
                  Pat.var({loc: default_loc^, txt: "make"}),
                  Exp.fun_(
                    Nolabel,
                    None,
                    Pat.var({loc: default_loc^, txt: "props"}),
                    Exp.apply(
                      Exp.ident({
                        loc: default_loc^,
                        txt: Ldot(Lident("React"), "createElement"),
                      }),
                      [
                        (
                          Nolabel,
                          Exp.letmodule(
                            {loc: default_loc^, txt: "Comp"},
                            Mod.unpack(
                              Exp.apply(
                                Exp.ident({
                                  loc: default_loc^,
                                  txt:
                                    Ldot(
                                      Lident("BootloaderResource"),
                                      "read",
                                    ),
                                }),
                                [
                                  (
                                    Nolabel,
                                    Exp.ident({
                                      loc: default_loc^,
                                      txt: Lident("reasonResource"),
                                    }),
                                  ),
                                ],
                              ),
                            ),
                            Exp.ident({
                              loc: default_loc^,
                              txt: Ldot(Lident("Comp"), "make"),
                            }),
                          ),
                        ),
                        (
                          Nolabel,
                          Exp.ident({
                            loc: default_loc^,
                            txt: Lident("props"),
                          }),
                        ),
                      ],
                    ),
                  ),
                ),
              ],
            ),
          ]),
      }
    )
  | _ => default_mapper.module_expr(mapper, module_expr)
  };

let () =
  Ast_mapper.register("lazyLoad", _argv =>
    {...default_mapper, structure, expr, module_expr}
  );