open GenFlowCommon;

type t =
  | IdentC
  | OptionC(t)
  | ArrayC(t)
  | RecordC(fieldsC)
  | FunctionC((list(groupedArgConverter), t))
and groupedArgConverter =
  | ArgConverter(label, t)
  | GroupConverter(list((string, t)))
and fieldsC = list((string, t));

let rec toString = converter =>
  switch (converter) {
  | IdentC => "id"
  | OptionC(c) => "option(" ++ toString(c) ++ ")"
  | ArrayC(c) => "array(" ++ toString(c) ++ ")"
  | RecordC(fieldsC) =>
    "{"
    ++ (
      fieldsC
      |> List.map(((lbl, c)) => lbl ++ ":" ++ (c |> toString))
      |> String.concat(", ")
    )
    ++ "}"
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

let rec typToConverter_ =
        (~exportTypeMap: StringMap.t((list(string), typ)), typ) =>
  switch (typ) {
  | Ident(s, typeArguments) =>
    try (
      {
        let (typeVars, t) = exportTypeMap |> StringMap.find(s);
        let pairs =
          try (List.combine(typeVars, typeArguments)) {
          | Invalid_argument(_) => []
          };

        let f = typeVar =>
          switch (pairs |> List.find(((typeVar1, _)) => typeVar == typeVar1)) {
          | (_, typeArgument) => Some(typeArgument)
          | exception Not_found => None
          };
        t |> TypeVars.substitute(~f) |> typToConverter_(~exportTypeMap);
      }
    ) {
    | Not_found => IdentC
    }
  | TypeVar(_) => IdentC
  | Option(t) => OptionC(t |> typToConverter_(~exportTypeMap))
  | Array(t) =>
    let converter = t |> typToConverter_(~exportTypeMap);
    converter == IdentC ? IdentC : ArrayC(converter);
  | Object(_) => IdentC
  | Record(fields) =>
    RecordC(
      fields
      |> List.map(((lbl, optionalness, t)) =>
           (
             lbl,
             (optionalness == Mandatory ? t : Option(t))
             |> typToConverter_(~exportTypeMap),
           )
         ),
    )
  | Function({argTypes, retType}) =>
    let argConverters =
      argTypes |> List.map(typToGroupedArgConverter_(~exportTypeMap));
    let retConverter = retType |> typToConverter_(~exportTypeMap);
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

let rec converterIsIdentity = (~toJS, converter) =>
  switch (converter) {
  | IdentC => true

  | OptionC(c) =>
    if (toJS) {
      c |> converterIsIdentity(~toJS);
    } else {
      false;
    }

  | ArrayC(c) => c |> converterIsIdentity(~toJS)

  | RecordC(_) => false

  | FunctionC((groupedArgConverters, resultConverter)) =>
    resultConverter
    |> converterIsIdentity(~toJS)
    && groupedArgConverters
    |> List.for_all(groupedArgConverter =>
         switch (groupedArgConverter) {
         | ArgConverter(label, argConverter) =>
           label == Nolabel
           && argConverter
           |> converterIsIdentity(~toJS=!toJS)
         | GroupConverter(_) => false
         }
       )
  };

let rec apply = (~converter, ~toJS, value) =>
  switch (converter) {
  | _ when converter |> converterIsIdentity(~toJS) => value

  | IdentC => value

  | OptionC(c) =>
    if (toJS) {
      value |> apply(~converter=c, ~toJS);
    } else {
      EmitText.parens([
        value
        ++ " == null ? undefined : "
        ++ (value |> apply(~converter=c, ~toJS)),
      ]);
    }

  | ArrayC(c) =>
    value
    ++ ".map(function _element(x) { return "
    ++ ("x" |> apply(~converter=c, ~toJS))
    ++ "})"

  | RecordC(fieldsC) =>
    if (toJS) {
      let fieldValues =
        fieldsC
        |> List.mapi((i, (lbl, fieldConverter)) =>
             lbl
             ++ ":"
             ++ (
               value
               ++ "["
               ++ string_of_int(i)
               ++ "]"
               |> apply(~converter=fieldConverter, ~toJS)
             )
           )
        |> String.concat(", ");
      "{" ++ fieldValues ++ "}";
    } else {
      let fieldValues =
        fieldsC
        |> List.map(((lbl, fieldConverter)) =>
             value ++ "." ++ lbl |> apply(~converter=fieldConverter, ~toJS)
           )
        |> String.concat(", ");
      "[" ++ fieldValues ++ "]";
    }

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