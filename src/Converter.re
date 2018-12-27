open GenTypeCommon;

type t =
  | ArrayC(t)
  | CircularC(string, t)
  | EnumC(enumC)
  | FunctionC(list(groupedArgConverter), t)
  | IdentC
  | NullableC(t)
  | ObjectC(fieldsC)
  | OptionC(t)
  | RecordC(fieldsC)
  | TupleC(list(t))
and groupedArgConverter =
  | ArgConverter(label, t)
  | GroupConverter(list((string, t)))
and fieldsC = list((string, t))
and enumC = {
  cases: list(case),
  obj: option((string, t)),
  toJS: string,
  toRE: string,
};

let rec toString = converter =>
  switch (converter) {
  | ArrayC(c) => "array(" ++ toString(c) ++ ")"

  | CircularC(s, c) => "circular(" ++ s ++ " " ++ toString(c) ++ ")"

  | EnumC({cases, obj, _}) =>
    "enum("
    ++ (
      cases
      |> List.map(case => case.labelJS |> labelJSToString)
      |> String.concat(", ")
    )
    ++ (
      switch (obj) {
      | None => ""
      | Some((_, c)) => ", " ++ (c |> toString)
      }
    )
    ++ ")"

  | FunctionC(groupedArgConverters, c) =>
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

  | IdentC => "id"

  | NullableC(c) => "nullable(" ++ toString(c) ++ ")"

  | ObjectC(fieldsC)
  | RecordC(fieldsC) =>
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

  | OptionC(c) => "option(" ++ toString(c) ++ ")"
  | TupleC(innerTypesC) =>
    "[" ++ (innerTypesC |> List.map(toString) |> String.concat(", ")) ++ "]"
  };

let typToConverterOpaque =
    (
      ~config,
      ~exportTypeMap: CodeItem.exportTypeMap,
      ~exportTypeMapFromOtherFiles,
      ~typeNameIsInterface,
      typ,
    ) => {
  let circular = ref("");
  let rec visit = (~visited: StringSet.t, typ) =>
    switch (typ) {
    | Array(t, _) =>
      let (converter, opaque) = t |> visit(~visited);
      (ArrayC(converter), opaque);

    | Enum(enum) => (
        EnumC({
          cases: enum.cases,
          obj:
            switch (enum.obj) {
            | None => None
            | Some((label, t)) => Some((label, t |> visit(~visited) |> fst))
            },
          toJS: enum.toJS,
          toRE: enum.toRE,
        }),
        false,
      )

    | Function({argTypes, retType, _}) =>
      let argConverters =
        argTypes |> List.map(typToGroupedArgConverter(~visited));
      let (retConverter, _) = retType |> visit(~visited);
      (FunctionC(argConverters, retConverter), false);

    | GroupOfLabeledArgs(_) => (IdentC, true)

    | Ident(s, typeArguments) =>
      if (visited |> StringSet.mem(s)) {
        circular := s;
        (IdentC, false);
      } else {
        let visited = visited |> StringSet.add(s);
        switch (
          try (exportTypeMap |> StringMap.find(s)) {
          | Not_found => exportTypeMapFromOtherFiles |> StringMap.find(s)
          }
        ) {
        | {annotation: GenTypeOpaque, _} => (IdentC, true)
        | {annotation: NoGenType, _} => (IdentC, true)
        | {typeVars, typ, _} =>
          let pairs =
            try (List.combine(typeVars, typeArguments)) {
            | Invalid_argument(_) => []
            };

          let f = typeVar =>
            switch (
              pairs |> List.find(((typeVar1, _)) => typeVar == typeVar1)
            ) {
            | (_, typeArgument) => Some(typeArgument)
            | exception Not_found => None
            };
          (typ |> TypeVars.substitute(~f) |> visit(~visited) |> fst, false);
        | exception Not_found =>
          let opaqueUnlessBase =
            !(typ == booleanT || typ == numberT || typ == stringT);
          (IdentC, opaqueUnlessBase);
        };
      }

    | Nullable(t) =>
      let (converter, opaque) = t |> visit(~visited);
      (NullableC(converter), opaque);

    | Object(fields) => (
        ObjectC(
          fields
          |> List.map(({name, optional, typ, _}) =>
               (
                 name,
                 (optional == Mandatory ? typ : Option(typ))
                 |> visit(~visited)
                 |> fst,
               )
             ),
        ),
        false,
      )

    | Option(t) =>
      let (converter, opaque) = t |> visit(~visited);
      (OptionC(converter), opaque);

    | Record(fields) => (
        RecordC(
          fields
          |> List.map(({name, optional, typ, _}) =>
               (
                 name,
                 (optional == Mandatory ? typ : Option(typ))
                 |> visit(~visited)
                 |> fst,
               )
             ),
        ),
        false,
      )

    | Tuple(innerTypes) =>
      let (innerConversions, opaques) =
        innerTypes |> List.map(visit(~visited)) |> List.split;
      (TupleC(innerConversions), opaques |> List.mem(true));

    | TypeVar(_) => (IdentC, true)

    | Variant(_) => (IdentC, true)
    }
  and typToGroupedArgConverter = (~visited, typ) =>
    switch (typ) {
    | GroupOfLabeledArgs(fields) =>
      GroupConverter(
        fields
        |> List.map(({name, typ, _}) =>
             (name, typ |> visit(~visited) |> fst)
           ),
      )
    | _ => ArgConverter(Nolabel, typ |> visit(~visited) |> fst)
    };

  let (converter, opaque) = typ |> visit(~visited=StringSet.empty);
  if (Debug.converter^) {
    logItem(
      "Converter typ:%s converter:%s\n",
      typ |> EmitTyp.typToString(~config, ~typeNameIsInterface),
      converter |> toString,
    );
  };
  let finalConverter =
    circular^ != "" ? CircularC(circular^, converter) : converter;
  (finalConverter, opaque);
};

let typToConverter =
    (
      ~config,
      ~exportTypeMap,
      ~exportTypeMapFromOtherFiles,
      ~typeNameIsInterface,
      typ,
    ) =>
  typ
  |> typToConverterOpaque(
       ~config,
       ~exportTypeMap,
       ~exportTypeMapFromOtherFiles,
       ~typeNameIsInterface,
     )
  |> fst;

let rec converterIsIdentity = (~toJS, converter) =>
  switch (converter) {
  | ArrayC(c) => c |> converterIsIdentity(~toJS)

  | CircularC(_, c) => c |> converterIsIdentity(~toJS)

  | EnumC(_) => false

  | FunctionC(groupedArgConverters, resultConverter) =>
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

  | IdentC => true

  | NullableC(c) => c |> converterIsIdentity(~toJS)

  | ObjectC(fieldsC) =>
    fieldsC
    |> List.for_all(((_, c)) =>
         switch (c) {
         | OptionC(c1) => c1 |> converterIsIdentity(~toJS)
         | _ => c |> converterIsIdentity(~toJS)
         }
       )

  | OptionC(c) =>
    if (toJS) {
      c |> converterIsIdentity(~toJS);
    } else {
      false;
    }

  | RecordC(_) => false
  | TupleC(innerTypesC) =>
    innerTypesC |> List.for_all(converterIsIdentity(~toJS))
  };

let rec apply = (~config, ~converter, ~enumTables, ~nameGen, ~toJS, value) =>
  switch (converter) {
  | _ when converter |> converterIsIdentity(~toJS) => value

  | ArrayC(c) =>
    let x = "ArrayItem" |> EmitText.name(~nameGen);
    value
    ++ ".map(function _element("
    ++ (x |> EmitTyp.ofTypeAnyTS(~config))
    ++ ") { return "
    ++ (x |> apply(~config, ~converter=c, ~enumTables, ~nameGen, ~toJS))
    ++ "})";

  | CircularC(s, c) =>
    "\n/* WARNING: circular type "
    ++ s
    ++ ". Only shallow converter applied. */\n  "
    ++ value
    |> apply(~config, ~converter=c, ~enumTables, ~nameGen, ~toJS)

  | EnumC({cases: [case], obj: None, _}) =>
    toJS ?
      case.labelJS |> labelJSToString : case.label |> Runtime.emitVariantLabel

  | EnumC(enum) =>
    let table = toJS ? enum.toJS : enum.toRE;
    Hashtbl.replace(enumTables, table, (enum, toJS));
    let convertToString =
      !toJS
      && enum.cases
      |> List.exists(({labelJS}) =>
           labelJS == BoolLabel(true) || labelJS == BoolLabel(false)
         ) ?
        ".toString()" : "";
    let accessTable = table ++ EmitText.array([value ++ convertToString]);
    switch (enum.obj) {
    | None => accessTable
    | Some((label, objConverter)) =>
      if (toJS) {
        "("
        ++ (value |> EmitText.typeOfObject)
        ++ " ? "
        ++ (
          value
          |> Runtime.emitVariantGetPayload
          |> apply(
               ~config,
               ~converter=objConverter,
               ~enumTables,
               ~nameGen,
               ~toJS,
             )
        )
        ++ " : "
        ++ accessTable
        ++ ")";
      } else {
        "("
        ++ (value |> EmitText.typeOfObject)
        ++ " ? "
        ++ (
          value
          |> apply(
               ~config,
               ~converter=objConverter,
               ~enumTables,
               ~nameGen,
               ~toJS,
             )
          |> Runtime.emitVariantWithPayload(~label)
        )
        ++ " : "
        ++ accessTable
        ++ ")";
      }
    };

  | FunctionC(groupedArgConverters, resultConverter) =>
    let resultName = EmitText.resultName(~nameGen);
    let mkReturn = x =>
      "const "
      ++ resultName
      ++ " = "
      ++ x
      ++ "; return "
      ++ (
        resultName
        |> apply(
             ~config,
             ~converter=resultConverter,
             ~enumTables,
             ~nameGen,
             ~toJS,
           )
      );
    let convertedArgs = {
      let convertArg = (i, groupedArgConverter) =>
        switch (groupedArgConverter) {
        | ArgConverter(lbl, argConverter) =>
          let varName =
            switch (lbl) {
            | Nolabel => i + 1 |> EmitText.argi(~nameGen)
            | Label(l)
            | OptLabel(l) => l |> EmitText.arg(~nameGen)
            };
          let notToJS = !toJS;
          (
            varName |> EmitTyp.ofTypeAnyTS(~config),
            varName
            |> apply(
                 ~config,
                 ~converter=argConverter,
                 ~enumTables,
                 ~nameGen,
                 ~toJS=notToJS,
               ),
          );
        | GroupConverter(groupConverters) =>
          let notToJS = !toJS;
          if (toJS) {
            let varName = i + 1 |> EmitText.argi(~nameGen);
            (
              varName |> EmitTyp.ofTypeAnyTS(~config),
              groupConverters
              |> List.map(((s, argConverter)) =>
                   varName
                   ++ "."
                   ++ s
                   |> apply(
                        ~config,
                        ~converter=argConverter,
                        ~enumTables,
                        ~nameGen,
                        ~toJS=notToJS,
                      )
                 )
              |> String.concat(", "),
            );
          } else {
            let varNames =
              groupConverters
              |> List.map(((s, _argConverter)) =>
                   s |> EmitText.arg(~nameGen)
                 );

            let varNamesArr = varNames |> Array.of_list;
            let fieldValues =
              groupConverters
              |> List.mapi((i, (s, argConverter)) =>
                   s
                   ++ ":"
                   ++ (
                     varNamesArr[i]
                     |> apply(
                          ~config,
                          ~converter=argConverter,
                          ~enumTables,
                          ~nameGen,
                          ~toJS=notToJS,
                        )
                   )
                 )
              |> String.concat(", ");
            (varNames |> String.concat(", "), "{" ++ fieldValues ++ "}");
          };
        };
      groupedArgConverters |> List.mapi(convertArg);
    };
    let mkBody = args => value |> EmitText.funCall(~args) |> mkReturn;
    EmitText.funDef(~args=convertedArgs, ~mkBody, "");

  | IdentC => value

  | NullableC(c) =>
    EmitText.parens([
      value
      ++ " == null ? "
      ++ value
      ++ " : "
      ++ (value |> apply(~config, ~converter=c, ~enumTables, ~nameGen, ~toJS)),
    ])

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
                  ~config,
                  ~converter=fieldConverter |> simplifyFieldConverted,
                  ~enumTables,
                  ~nameGen,
                  ~toJS,
                )
           )
         )
      |> String.concat(", ");
    "{" ++ fieldValues ++ "}";

  | OptionC(c) =>
    if (toJS) {
      EmitText.parens([
        value
        ++ " == null ? "
        ++ value
        ++ " : "
        ++ (
          value |> apply(~config, ~converter=c, ~enumTables, ~nameGen, ~toJS)
        ),
      ]);
    } else {
      EmitText.parens([
        value
        ++ " == null ? undefined : "
        ++ (
          value |> apply(~config, ~converter=c, ~enumTables, ~nameGen, ~toJS)
        ),
      ]);
    }

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
                    ~config,
                    ~converter=fieldConverter |> simplifyFieldConverted,
                    ~enumTables,
                    ~nameGen,
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
                  ~config,
                  ~converter=fieldConverter |> simplifyFieldConverted,
                  ~enumTables,
                  ~nameGen,
                  ~toJS,
                )
           )
        |> String.concat(", ");
      "[" ++ fieldValues ++ "]";
    };
  | TupleC(innerTypesC) =>
    "["
    ++ (
      innerTypesC
      |> List.mapi((i, c) =>
           value
           ++ "["
           ++ string_of_int(i)
           ++ "]"
           |> apply(~config, ~converter=c, ~enumTables, ~nameGen, ~toJS)
         )
      |> String.concat(", ")
    )
    ++ "]"
  };

let toJS = (~config, ~converter, ~enumTables, ~nameGen, value) =>
  value |> apply(~config, ~converter, ~enumTables, ~nameGen, ~toJS=true);

let toReason = (~config, ~converter, ~enumTables, ~nameGen, value) =>
  value |> apply(~config, ~converter, ~enumTables, ~nameGen, ~toJS=false);