[@bs.module]
[@deprecated
  "Please use this syntax to guarantee safe usage: [%requireCond(`gk, \"gk_name\", ConditionalModule)]"
]
external make:
  (
    [@bs.string] [ | [@bs.as "qe.bool"] `qeBool | [@bs.as "gk"] `gk],
    string,
    string
  ) =>
  Js.Nullable.t('a) =
  "requireCond";

[@bs.module]
[@deprecated
  "Please use this syntax to guarantee safe usage: [%requireCond(`gk, \"gk_name\", {\"true\": ModuleA, \"false\": ModuleB})]"
]
external either:
  (
    [@bs.string] [ | [@bs.as "qe.bool"] `qeBool | [@bs.as "gk"] `gk],
    string,
    {
      .
      "true": string,
      "false": string,
    }
  ) =>
  'b =
  "requireCond";