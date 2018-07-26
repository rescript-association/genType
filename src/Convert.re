let rec toString = converter =>
  switch (converter) {
  | CodeItem.Unit => "unit"
  | Identity => "id"
  | OptionalArgument(c) => "optionalArgument(" ++ toString(c) ++ ")"
  | Option(c) => "option(" ++ toString(c) ++ ")"
  | Fn((args, c)) =>
    let labelToString = label =>
      switch (label) {
      | NamedArgs.Nolabel => "-"
      | Label(l) => l
      | OptLabel(l) => "?" ++ l
      };
    "fn("
    ++ (
      args
      |> List.map(((label, conv)) =>
           "(~" ++ labelToString(label) ++ ":" ++ toString(conv) ++ ")"
         )
      |> String.concat(", ")
    )
    ++ " -> "
    ++ toString(c)
    ++ ")";
  };

let rec apply = (~converter, ~toJS, value) =>
  switch (converter) {
  | CodeItem.Unit
  | Identity => value

  | OptionalArgument(c) => apply(~converter=c, ~toJS, value)

  | Option(c) =>
    let optionToNullable = x => x;
    let nullableToOption = x =>
      Emit.parens([x ++ " === null ? undefined : " ++ x]);
    value
    |> apply(~converter=c, ~toJS)
    |> (toJS ? optionToNullable : nullableToOption);

  | Fn((args, resultConverter))
      when
        args
        |> List.for_all(((_label, argConverter)) =>
             argConverter == CodeItem.Identity
           ) =>
    value |> apply(~converter=resultConverter, ~toJS)

  | Fn((args, resultConverter)) =>
    let resultVar = "result";
    let mkReturn = x =>
      "const "
      ++ resultVar
      ++ " = "
      ++ x
      ++ "; return "
      ++ apply(~converter=resultConverter, ~toJS, resultVar);
    let convertedArgs = {
      let convertArg = (i, (lbl, argConverter)) => {
        let varName =
          switch (lbl) {
          | NamedArgs.Nolabel => Emit.argi(i + 1)
          | Label(l)
          | OptLabel(l) => Emit.arg(l)
          };
        let notToJS = !toJS;
        (varName, varName |> apply(~converter=argConverter, ~toJS=notToJS));
      };
      args |> List.mapi(convertArg);
    };
    let mkBody = args => value |> Emit.funCall(~args) |> mkReturn;
    Emit.funDef(~args=convertedArgs, ~mkBody, "");
  };

let toJS = (~converter, value) => value |> apply(~converter, ~toJS=true);

let toReason = (~converter, value) =>
  value |> apply(~converter, ~toJS=false);