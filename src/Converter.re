open GenFlowCommon;

type t =
  | Identity
  | Option(t)
  | Fn((list(groupedArgConverter), t))
and groupedArgConverter =
  | ArgConverter(label, t)
  | GroupConverter(list((string, t)));

let rec toString = converter =>
  switch (converter) {
  | Identity => "id"
  | Option(c) => "option(" ++ toString(c) ++ ")"
  | Fn((groupedArgConverters, c)) =>
    let labelToString = label =>
      switch (label) {
      | Nolabel => "_"
      | Label(_) => "~l"
      | OptLabel(l) => "~?" ++ l
      };
    "fn("
    ++ (
      groupedArgConverters
      |> List.map(groupedArgConverter =>
           switch (groupedArgConverter) {
           | ArgConverter(label, conv) =>
             "(" ++ labelToString(label) ++ ":" ++ toString(conv) ++ ")"
           | GroupConverter(groupConverters) =>
             "{|"
             ++ (
               groupConverters
               |> List.map(((s, argConverter)) =>
                    s ++ ":" ++ toString(argConverter)
                  )
               |> String.concat(", ")
             )
             ++ "|}"
           }
         )
      |> String.concat(", ")
    )
    ++ " -> "
    ++ toString(c)
    ++ ")";
  };

let rec typToConverter = typ =>
  switch (typ) {
  | TypeVar(_) => Identity
  | Ident(_, _) => Identity
  | Optional(t) => Option(t |> typToConverter)
  | ObjectType(_) => Identity
  | Arrow(_generics, args, resultType) =>
    let argConverters = args |> List.map(typToGroupedArgConverter);
    let retConverter = resultType |> typToConverter;
    if (retConverter == Identity
        && argConverters
        |> List.for_all(converter =>
             converter == ArgConverter(Nolabel, Identity)
           )) {
      Identity;
    } else {
      Fn((argConverters, retConverter));
    };
  }
and typToGroupedArgConverter = typ =>
  switch (typ) {
  | ObjectType(fields) =>
    GroupConverter(
      fields
      |> List.map(((s, _optionalness, t)) => (s, t |> typToConverter)),
    )
  | _ => ArgConverter(Nolabel, typ |> typToConverter)
  };

let rec apply = (~converter, ~toJS, value) =>
  switch (converter) {
  | Identity => value

  | Option(c) =>
    let optionToNullable = x => x;
    let nullableToOption = x =>
      Emit.parens([x ++ " === null ? undefined : " ++ x]);
    value
    |> apply(~converter=c, ~toJS)
    |> (toJS ? optionToNullable : nullableToOption);

  | Fn((groupedArgConverters, resultConverter))
      when
        groupedArgConverters
        |> List.for_all(groupedArgConverter =>
             switch (groupedArgConverter) {
             | ArgConverter(label, argConverter) =>
               label == Nolabel && argConverter == Identity
             | GroupConverter(_) => false
             }
           ) =>
    value |> apply(~converter=resultConverter, ~toJS)

  | Fn((groupedArgConverters, resultConverter)) =>
    let resultVar = "result";
    let mkReturn = x =>
      "const "
      ++ resultVar
      ++ " = "
      ++ x
      ++ "; return "
      ++ apply(~converter=resultConverter, ~toJS, resultVar);
    let convertedArgs = {
      let convertArg = (i, groupedArgConverter) =>
        switch (groupedArgConverter) {
        | ArgConverter(lbl, argConverter) =>
          let varName =
            switch (lbl) {
            | Nolabel => Emit.argi(i + 1)
            | Label(l)
            | OptLabel(l) => Emit.arg(l)
            };
          let notToJS = !toJS;
          (
            varName,
            varName |> apply(~converter=argConverter, ~toJS=notToJS),
          );
        | GroupConverter(groupConverters) =>
          let notToJS = !toJS;
          if (toJS) {
            let varName = Emit.argi(i + 1);
            (
              varName,
              groupConverters
              |> List.map(((s, argConverter)) =>
                   varName
                   ++ "."
                   ++ s
                   |> apply(~converter=argConverter, ~toJS=notToJS)
                 )
              |> String.concat(", "),
            );
          } else {
            let varNames =
              groupConverters
              |> List.map(((s, _argConverter)) => Emit.arg(s))
              |> String.concat(", ");
            let fieldValues =
              groupConverters
              |> List.map(((s, argConverter)) =>
                   s
                   ++ ":"
                   ++ (
                     Emit.arg(s)
                     |> apply(~converter=argConverter, ~toJS=notToJS)
                   )
                 )
              |> String.concat(", ");
            (varNames, "{" ++ fieldValues ++ "}");
          };
        };
      groupedArgConverters |> List.mapi(convertArg);
    };
    let mkBody = args => value |> Emit.funCall(~args) |> mkReturn;
    Emit.funDef(~args=convertedArgs, ~mkBody, "");
  };

let toJS = (~converter, value) => value |> apply(~converter, ~toJS=true);

let toReason = (~converter, value) =>
  value |> apply(~converter, ~toJS=false);