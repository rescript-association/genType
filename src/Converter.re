open GenTypeCommon;

type t =
  | IdentC
  | OptionC(t)
  | NullableC(t)
  | ArrayC(t)
  | ObjectC(fieldsC)
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
  | NullableC(c) => "nullable(" ++ toString(c) ++ ")"
  | ArrayC(c) => "array(" ++ toString(c) ++ ")"
  | RecordC(fieldsC)
  | ObjectC(fieldsC) =>
    let dot =
      switch (converter) {
      | ObjectC(_) => ". "
      | _ => ""
      };
    "{"
    ++ dot
    ++ (
      fieldsC
      |> List.map(((lbl, c)) => lbl ++ ":" ++ (c |> toString))
      |> String.concat(", ")
    )
    ++ "}";
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
        (
          ~exportTypeMap: StringMap.t((list(string), typ)),
          ~typesFromOtherFiles: StringMap.t((list(string), typ)),
          typ,
        ) =>
  switch (typ) {
  | Array(t) =>
    let converter =
      t |> typToConverter_(~exportTypeMap, ~typesFromOtherFiles);
    ArrayC(converter);

  | Enum(_) => IdentC

  | Function({argTypes, retType, _}) =>
    let argConverters =
      argTypes
      |> List.map(
           typToGroupedArgConverter_(~exportTypeMap, ~typesFromOtherFiles),
         );
    let retConverter =
      retType |> typToConverter_(~exportTypeMap, ~typesFromOtherFiles);
    FunctionC((argConverters, retConverter));

  | GroupOfLabeledArgs(_) => IdentC

  | Ident(s, typeArguments) =>
    try (
      {
        let (typeVars, t) =
          try (exportTypeMap |> StringMap.find(s)) {
          | Not_found => typesFromOtherFiles |> StringMap.find(s)
          };
        let pairs =
          try (List.combine(typeVars, typeArguments)) {
          | Invalid_argument(_) => []
          };

        let f = typeVar =>
          switch (pairs |> List.find(((typeVar1, _)) => typeVar == typeVar1)) {
          | (_, typeArgument) => Some(typeArgument)
          | exception Not_found => None
          };
        t
        |> TypeVars.substitute(~f)
        |> typToConverter_(~exportTypeMap, ~typesFromOtherFiles);
      }
    ) {
    | Not_found => IdentC
    }

  | Nullable(t) =>
    let converter =
      t |> typToConverter_(~exportTypeMap, ~typesFromOtherFiles);
    NullableC(converter);

  | Object(fields) =>
    ObjectC(
      fields
      |> List.map(((lbl, optionalness, t)) =>
           (
             lbl,
             (optionalness == Mandatory ? t : Option(t))
             |> typToConverter_(~exportTypeMap, ~typesFromOtherFiles),
           )
         ),
    )

  | Option(t) =>
    OptionC(t |> typToConverter_(~exportTypeMap, ~typesFromOtherFiles))

  | Record(fields) =>
    RecordC(
      fields
      |> List.map(((lbl, optionalness, t)) =>
           (
             lbl,
             (optionalness == Mandatory ? t : Option(t))
             |> typToConverter_(~exportTypeMap, ~typesFromOtherFiles),
           )
         ),
    )

  | TypeVar(_) => IdentC
  }
and typToGroupedArgConverter_ = (~exportTypeMap, ~typesFromOtherFiles, typ) =>
  switch (typ) {
  | GroupOfLabeledArgs(fields) =>
    GroupConverter(
      fields
      |> List.map(((s, _optionalness, t)) =>
           (s, t |> typToConverter_(~exportTypeMap, ~typesFromOtherFiles))
         ),
    )
  | _ =>
    ArgConverter(
      Nolabel,
      typ |> typToConverter_(~exportTypeMap, ~typesFromOtherFiles),
    )
  };

let typToConverter = (~language, ~exportTypeMap, ~typesFromOtherFiles, typ) => {
  let converter =
    typ |> typToConverter_(~exportTypeMap, ~typesFromOtherFiles);
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

  | NullableC(c) => c |> converterIsIdentity(~toJS)

  | ArrayC(c) => c |> converterIsIdentity(~toJS)

  | ObjectC(fieldsC) =>
    fieldsC
    |> List.for_all(((_, c)) =>
         switch (c) {
         | OptionC(c1) => c1 |> converterIsIdentity(~toJS)
         | _ => c |> converterIsIdentity(~toJS)
         }
       )

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
      EmitText.parens([
        value
        ++ " == null ? "
        ++ value
        ++ " : "
        ++ (value |> apply(~converter=c, ~toJS)),
      ]);
    } else {
      EmitText.parens([
        value
        ++ " == null ? undefined : "
        ++ (value |> apply(~converter=c, ~toJS)),
      ]);
    }

  | NullableC(c) =>
    EmitText.parens([
      value
      ++ " == null ? "
      ++ value
      ++ " : "
      ++ (value |> apply(~converter=c, ~toJS)),
    ])

  | ArrayC(c) =>
    value
    ++ ".map(function _element(x) { return "
    ++ ("x" |> apply(~converter=c, ~toJS))
    ++ "})"

  | ObjectC(fieldsC) =>
    let simplifyFieldConverted = fieldConverter =>
      switch (fieldConverter) {
      | OptionC(converter1) when converter1 |> converterIsIdentity(~toJS) =>
        IdentC
      | _ => fieldConverter
      };
    let fieldValues =
      fieldsC
      |> List.map(((lbl, fieldConverter)) =>
           lbl
           ++ ":"
           ++ (
             value
             ++ "."
             ++ lbl
             |> apply(
                  ~converter=fieldConverter |> simplifyFieldConverted,
                  ~toJS,
                )
           )
         )
      |> String.concat(", ");
    "{" ++ fieldValues ++ "}";

  | RecordC(fieldsC) =>
    let simplifyFieldConverted = fieldConverter =>
      switch (fieldConverter) {
      | OptionC(converter1) when converter1 |> converterIsIdentity(~toJS) =>
        IdentC
      | _ => fieldConverter
      };
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
               |> apply(
                    ~converter=fieldConverter |> simplifyFieldConverted,
                    ~toJS,
                  )
             )
           )
        |> String.concat(", ");
      "{" ++ fieldValues ++ "}";
    } else {
      let fieldValues =
        fieldsC
        |> List.map(((lbl, fieldConverter)) =>
             value
             ++ "."
             ++ lbl
             |> apply(
                  ~converter=fieldConverter |> simplifyFieldConverted,
                  ~toJS,
                )
           )
        |> String.concat(", ");
      "[" ++ fieldValues ++ "]";
    };

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