open GenFlowCommon;

type t =
  | IdentC
  | OptionC(t)
  | FunctionC((list(groupedArgConverter), t))
and groupedArgConverter =
  | ArgConverter(label, t)
  | GroupConverter(list((string, t)));

let rec toString = converter =>
  switch (converter) {
  | IdentC => "id"
  | OptionC(c) => "option(" ++ toString(c) ++ ")"
  | FunctionC((groupedArgConverters, c)) =>
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

let rec typToConverter_ = (~exportTypeMap: StringMap.t(typ), typ) =>
  switch (typ) {
  | TypeVar(_) => IdentC
  | Ident(s, []) =>
    try (
      {
        let t = exportTypeMap |> StringMap.find(s);
        t |> typToConverter_(~exportTypeMap);
      }
    ) {
    | Not_found => IdentC
    }
  | Ident(_, _) => IdentC
  | Option(t) => OptionC(t |> typToConverter_(~exportTypeMap))
  | Object(_) => IdentC
  | Record(_) => IdentC
  | Function(_generics, argTypes, resultType) =>
    let argConverters =
      argTypes |> List.map(typToGroupedArgConverter_(~exportTypeMap));
    let retConverter = resultType |> typToConverter_(~exportTypeMap);
    if (retConverter == IdentC
        && argConverters
        |> List.for_all(converter =>
             converter == ArgConverter(Nolabel, IdentC)
           )) {
      IdentC;
    } else {
      FunctionC((argConverters, retConverter));
    };
  }
and typToGroupedArgConverter_ = (~exportTypeMap, typ) =>
  switch (typ) {
  | Object(fields) =>
    GroupConverter(
      fields
      |> List.map(((s, _optionalness, t)) =>
           (s, t |> typToConverter_(~exportTypeMap))
         ),
    )
  | _ => ArgConverter(Nolabel, typ |> typToConverter_(~exportTypeMap))
  };

let typToConverter = (~language, ~exportTypeMap, typ) => {
  let converter = typ |> typToConverter_(~exportTypeMap);
  if (Debug.converter) {
    logItem(
      "Converter typ:%s converter:%s\n",
      typ |> EmitTyp.typToString(~language),
      converter |> toString,
    );
  };
  converter;
};

let rec apply = (~converter, ~toJS, value) =>
  switch (converter) {
  | IdentC => value

  | OptionC(c) =>
    let optionToNullable = x => x;
    let nullableToOption = x =>
      EmitText.parens([x ++ " === null ? undefined : " ++ x]);
    value
    |> apply(~converter=c, ~toJS)
    |> (toJS ? optionToNullable : nullableToOption);

  | FunctionC((groupedArgConverters, resultConverter))
      when
        groupedArgConverters
        |> List.for_all(groupedArgConverter =>
             switch (groupedArgConverter) {
             | ArgConverter(label, argConverter) =>
               label == Nolabel && argConverter == IdentC
             | GroupConverter(_) => false
             }
           ) =>
    value |> apply(~converter=resultConverter, ~toJS)

  | FunctionC((groupedArgConverters, resultConverter)) =>
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
            | Nolabel => EmitText.argi(i + 1)
            | Label(l)
            | OptLabel(l) => EmitText.arg(l)
            };
          let notToJS = !toJS;
          (
            varName,
            varName |> apply(~converter=argConverter, ~toJS=notToJS),
          );
        | GroupConverter(groupConverters) =>
          let notToJS = !toJS;
          if (toJS) {
            let varName = EmitText.argi(i + 1);
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
              |> List.map(((s, _argConverter)) => EmitText.arg(s))
              |> String.concat(", ");
            let fieldValues =
              groupConverters
              |> List.map(((s, argConverter)) =>
                   s
                   ++ ":"
                   ++ (
                     EmitText.arg(s)
                     |> apply(~converter=argConverter, ~toJS=notToJS)
                   )
                 )
              |> String.concat(", ");
            (varNames, "{" ++ fieldValues ++ "}");
          };
        };
      groupedArgConverters |> List.mapi(convertArg);
    };
    let mkBody = args => value |> EmitText.funCall(~args) |> mkReturn;
    EmitText.funDef(~args=convertedArgs, ~mkBody, "");
  };

let toJS = (~converter, value) => value |> apply(~converter, ~toJS=true);

let toReason = (~converter, value) =>
  value |> apply(~converter, ~toJS=false);