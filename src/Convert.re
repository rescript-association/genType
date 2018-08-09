open GenFlowCommon;

let rec apply = (~converter, ~toJS, value) =>
  switch (converter) {
  | Unit
  | Identity => value

  | OptionalArgument(c) => apply(~converter=c, ~toJS, value)

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