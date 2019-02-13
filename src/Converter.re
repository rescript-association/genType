open GenTypeCommon;

type t =
  | ArrayC(t)
  | CircularC(string, t)
  | FunctionC(list(groupedArgConverter), t)
  | IdentC
  | NullableC(t)
  | ObjectC(fieldsC)
  | OptionC(t)
  | RecordC(fieldsC)
  | TupleC(list(t))
  | VariantC(variantC)
and groupedArgConverter =
  | ArgConverter(t)
  | GroupConverter(list((string, optional, t)))
  | UnitConverter
and fieldsC = list((string, t))
and variantC = {
  noPayloads: list(case),
  withPayload: list((case, int, t)),
  polymorphic: bool,
  toJS: string,
  toRE: string,
  unboxed: bool,
};

let rec toString = converter =>
  switch (converter) {
  | ArrayC(c) => "array(" ++ toString(c) ++ ")"

  | CircularC(s, c) => "circular(" ++ s ++ " " ++ toString(c) ++ ")"

  | FunctionC(groupedArgConverters, c) =>
    "fn("
    ++ (
      groupedArgConverters
      |> List.map(groupedArgConverter =>
           switch (groupedArgConverter) {
           | ArgConverter(conv) => "(" ++ "_" ++ ":" ++ toString(conv) ++ ")"
           | GroupConverter(groupConverters) =>
             "{|"
             ++ (
               groupConverters
               |> List.map(((s, optional, argConverter)) =>
                    s
                    ++ (optional == Optional ? "?" : "")
                    ++ ":"
                    ++ toString(argConverter)
                  )
               |> String.concat(", ")
             )
             ++ "|}"
           | UnitConverter => "unit"
           }
         )
      |> String.concat(", ")
    )
    ++ " -> "
    ++ toString(c)
    ++ ")"

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

  | VariantC({noPayloads, withPayload, _}) =>
    "variant("
    ++ (
      (noPayloads |> List.map(case => case.labelJS |> labelJSToString))
      @ (
        withPayload
        |> List.map(((case, numArgs, c)) =>
             (case.labelJS |> labelJSToString)
             ++ ":"
             ++ string_of_int(numArgs)
             ++ ":"
             ++ (c |> toString)
           )
      )
      |> String.concat(", ")
    )
    ++ ")"
  };

let typToConverterNormalized =
    (
      ~config,
      ~exportTypeMap: CodeItem.exportTypeMap,
      ~exportTypeMapFromOtherFiles,
      ~typeNameIsInterface,
      typ,
    ) => {
  let circular = ref("");
  let expandOneLevel = typ =>
    switch (typ) {
    | Ident(s, _) =>
      switch (exportTypeMap |> StringMap.find(s)) {
      | (t: CodeItem.exportTypeItem) => t.typ
      | exception Not_found =>
        switch (exportTypeMapFromOtherFiles |> StringMap.find(s)) {
        | exception Not_found => typ
        | (t: CodeItem.exportTypeItem) => t.typ
        }
      }
    | _ => typ
    };
  let rec visit = (~visited: StringSet.t, typ) => {
    let normalized_ = Some(typ);
    switch (typ) {
    | Array(t, _) =>
      let (tConverter, tNormalized) = t |> visit(~visited);
      (ArrayC(tConverter), tNormalized == None ? None : normalized_);

    | Function({argTypes, retType, _}) =>
      let argConverters =
        argTypes |> List.map(typToGroupedArgConverter(~visited));
      let (retConverter, _) = retType |> visit(~visited);
      (FunctionC(argConverters, retConverter), normalized_);

    | GroupOfLabeledArgs(_) => (IdentC, None)

    | Ident(s, typeArguments) =>
      if (visited |> StringSet.mem(s)) {
        circular := s;
        (IdentC, normalized_);
      } else {
        let visited = visited |> StringSet.add(s);
        switch (
          try (exportTypeMap |> StringMap.find(s)) {
          | Not_found => exportTypeMapFromOtherFiles |> StringMap.find(s)
          }
        ) {
        | {annotation: GenTypeOpaque, _} => (IdentC, None)
        | {annotation: NoGenType, _} => (IdentC, None)
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
          (
            typ |> TypeVars.substitute(~f) |> visit(~visited) |> fst,
            normalized_,
          );
        | exception Not_found =>
          let isBaseType = typ == booleanT || typ == numberT || typ == stringT;
          (IdentC, isBaseType ? normalized_ : None);
        };
      }

    | Nullable(t) =>
      let (tConverter, tNormalized) = t |> visit(~visited);
      (NullableC(tConverter), tNormalized == None ? None : normalized_);

    | Object(_, fields) => (
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
        normalized_,
      )

    | Option(t) =>
      let (tConverter, tNormalized) = t |> visit(~visited);
      (OptionC(tConverter), tNormalized == None ? None : normalized_);

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
        normalized_,
      )

    | Tuple(innerTypes) =>
      let (innerConversions, normalizedList) =
        innerTypes |> List.map(visit(~visited)) |> List.split;
      let innerHasNone = normalizedList |> List.mem(None);
      (TupleC(innerConversions), innerHasNone ? None : normalized_);

    | TypeVar(_) => (IdentC, normalized_)

    | Variant(variant) =>
      let (withPayload, normalized, unboxed) =
        switch (variant.payloads) {
        | [] => ([], normalized_, variant.unboxed)
        | [(case, numArgs, t)] =>
          let converter = t |> visit(~visited) |> fst;
          let unboxed = t |> expandOneLevel |> typIsObject;
          let normalized =
            unboxed ?
              Some(Variant({...variant, unboxed: true})) : normalized_;
          ([(case, numArgs, converter)], normalized, unboxed);
        | [_, _, ..._] => (
            variant.payloads
            |> List.map(((case, numArgs, t)) => {
                 let converter = t |> visit(~visited) |> fst;
                 (case, numArgs, converter);
               }),
            normalized_,
            variant.unboxed,
          )
        };
      let converter =
        normalized == None ?
          IdentC :
          VariantC({
            noPayloads: variant.noPayloads,
            withPayload,
            polymorphic: variant.polymorphic,
            toJS: variant.toJS,
            toRE: variant.toRE,
            unboxed,
          });
      (converter, normalized);
    };
  }
  and typToGroupedArgConverter = (~visited, typ) =>
    switch (typ) {
    | GroupOfLabeledArgs(fields) =>
      GroupConverter(
        fields
        |> List.map(({name, typ, optional, _}) =>
             (name, optional, typ |> visit(~visited) |> fst)
           ),
      )
    | _ =>
      typ == unitT ?
        UnitConverter : ArgConverter(typ |> visit(~visited) |> fst)
    };

  let (converter, normalized) = typ |> visit(~visited=StringSet.empty);
  let finalConverter =
    circular^ != "" ? CircularC(circular^, converter) : converter;
  if (Debug.converter^) {
    logItem(
      "Converter %s typ:%s converter:%s\n",
      normalized == None ? " opaque " : "",
      typ |> EmitTyp.typToString(~config, ~typeNameIsInterface),
      finalConverter |> toString,
    );
  };
  (finalConverter, normalized);
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
  |> typToConverterNormalized(
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

  | FunctionC(groupedArgConverters, resultConverter) =>
    resultConverter
    |> converterIsIdentity(~toJS)
    && groupedArgConverters
    |> List.for_all(groupedArgConverter =>
         switch (groupedArgConverter) {
         | ArgConverter(argConverter) =>
           argConverter |> converterIsIdentity(~toJS=!toJS)
         | GroupConverter(_) => false
         | UnitConverter => toJS
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

  | VariantC(_) => false
  };

let rec apply =
        (
          ~config,
          ~converter,
          ~indent,
          ~nameGen,
          ~toJS,
          ~useCreateBucklescriptBlock,
          ~variantTables,
          value,
        ) =>
  switch (converter) {
  | _ when converter |> converterIsIdentity(~toJS) => value

  | ArrayC(c) =>
    let x = "ArrayItem" |> EmitText.name(~nameGen);
    value
    ++ ".map(function _element("
    ++ (x |> EmitTyp.ofTypeAnyTS(~config))
    ++ ") { return "
    ++ (
      x
      |> apply(
           ~config,
           ~converter=c,
           ~indent,
           ~nameGen,
           ~toJS,
           ~useCreateBucklescriptBlock,
           ~variantTables,
         )
    )
    ++ "})";

  | CircularC(s, c) =>
    "\n/* WARNING: circular type "
    ++ s
    ++ ". Only shallow converter applied. */\n  "
    ++ value
    |> apply(
         ~config,
         ~converter=c,
         ~indent,
         ~nameGen,
         ~toJS,
         ~useCreateBucklescriptBlock,
         ~variantTables,
       )

  | FunctionC(groupedArgConverters, resultConverter) =>
    let resultName = EmitText.resultName(~nameGen);
    let mkReturn = (~indent, x) => {
      let indent1 = indent |> Indent.more;
      "const "
      ++ resultName
      ++ " = "
      ++ x
      ++ ";"
      ++ Indent.break(~indent)
      ++ "return "
      ++ (
        resultName
        |> apply(
             ~config,
             ~converter=resultConverter,
             ~indent=indent1,
             ~nameGen,
             ~toJS,
             ~useCreateBucklescriptBlock,
             ~variantTables,
           )
      );
    };
    let convertedArgs = (~indent) => {
      let indent1 = indent |> Indent.more;
      let convertArg = (i, groupedArgConverter) =>
        switch (groupedArgConverter) {
        | ArgConverter(argConverter) =>
          let varName = i + 1 |> EmitText.argi(~nameGen);
          let notToJS = !toJS;
          (
            varName |> EmitTyp.ofTypeAnyTS(~config),
            varName
            |> apply(
                 ~config,
                 ~converter=argConverter,
                 ~indent=indent1,
                 ~nameGen,
                 ~toJS=notToJS,
                 ~useCreateBucklescriptBlock,
                 ~variantTables,
               ),
          );
        | GroupConverter(groupConverters) =>
          let notToJS = !toJS;
          if (toJS) {
            let varName = i + 1 |> EmitText.argi(~nameGen);
            (
              varName |> EmitTyp.ofTypeAnyTS(~config),
              groupConverters
              |> List.map(((s, optional, argConverter)) =>
                   varName
                   ++ "."
                   ++ s
                   |> apply(
                        ~config,
                        ~converter=
                          optional == Optional
                          && !(argConverter |> converterIsIdentity(~toJS)) ?
                            OptionC(argConverter) : argConverter,
                        ~indent=indent1,
                        ~nameGen,
                        ~toJS=notToJS,
                        ~useCreateBucklescriptBlock,
                        ~variantTables,
                      )
                 )
              |> String.concat(", "),
            );
          } else {
            let varNames =
              groupConverters
              |> List.map(((s, _optional, _argConverter)) =>
                   s |> EmitText.arg(~nameGen)
                 );

            let varNamesArr = varNames |> Array.of_list;
            let fieldValues =
              groupConverters
              |> List.mapi((i, (s, _optional, argConverter)) =>
                   s
                   ++ ":"
                   ++ (
                     varNamesArr[i]
                     |> apply(
                          ~config,
                          ~converter=argConverter,
                          ~indent=indent1,
                          ~nameGen,
                          ~toJS=notToJS,
                          ~useCreateBucklescriptBlock,
                          ~variantTables,
                        )
                   )
                 )
              |> String.concat(", ");
            (varNames |> String.concat(", "), "{" ++ fieldValues ++ "}");
          };
        | UnitConverter =>
          let varName = i + 1 |> EmitText.argi(~nameGen);
          (varName |> EmitTyp.ofTypeAnyTS(~config), varName);
        };
      groupedArgConverters |> List.mapi(convertArg);
    };
    let mkBody = (~indent, args) =>
      value |> EmitText.funCall(~args) |> mkReturn(~indent);
    EmitText.funDef(~args=convertedArgs, ~mkBody, ~indent, "");

  | IdentC => value

  | NullableC(c) =>
    EmitText.parens([
      value
      ++ " == null ? "
      ++ value
      ++ " : "
      ++ (
        value
        |> apply(
             ~config,
             ~converter=c,
             ~indent,
             ~nameGen,
             ~toJS,
             ~useCreateBucklescriptBlock,
             ~variantTables,
           )
      ),
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
                  ~indent,
                  ~nameGen,
                  ~toJS,
                  ~useCreateBucklescriptBlock,
                  ~variantTables,
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
          value
          |> apply(
               ~config,
               ~converter=c,
               ~indent,
               ~nameGen,
               ~toJS,
               ~useCreateBucklescriptBlock,
               ~variantTables,
             )
        ),
      ]);
    } else {
      EmitText.parens([
        value
        ++ " == null ? undefined : "
        ++ (
          value
          |> apply(
               ~config,
               ~converter=c,
               ~indent,
               ~nameGen,
               ~toJS,
               ~useCreateBucklescriptBlock,
               ~variantTables,
             )
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
                    ~indent,
                    ~nameGen,
                    ~toJS,
                    ~useCreateBucklescriptBlock,
                    ~variantTables,
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
                  ~indent,
                  ~nameGen,
                  ~toJS,
                  ~useCreateBucklescriptBlock,
                  ~variantTables,
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
           |> apply(
                ~config,
                ~converter=c,
                ~indent,
                ~nameGen,
                ~toJS,
                ~useCreateBucklescriptBlock,
                ~variantTables,
              )
         )
      |> String.concat(", ")
    )
    ++ "]"

  | VariantC({noPayloads: [case], withPayload: [], polymorphic, _}) =>
    toJS ?
      case.labelJS |> labelJSToString :
      case.label |> Runtime.emitVariantLabel(~polymorphic)

  | VariantC(variantC) =>
    let table = toJS ? variantC.toJS : variantC.toRE;
    if (variantC.noPayloads != []) {
      Hashtbl.replace(variantTables, table, (variantC, toJS));
    };
    let convertToString =
      !toJS
      && variantC.noPayloads
      |> List.exists(({labelJS}) =>
           labelJS == BoolLabel(true) || labelJS == BoolLabel(false)
         ) ?
        ".toString()" : "";
    let accessTable = v => table ++ EmitText.array([v ++ convertToString]);
    switch (variantC.withPayload) {
    | [] => value |> accessTable

    | [(case, numArgs, objConverter)] when variantC.unboxed =>
      let casesWithPayload = (~indent) =>
        if (toJS) {
          value
          |> Runtime.emitVariantGetPayload(
               ~numArgs,
               ~polymorphic=variantC.polymorphic,
             )
          |> apply(
               ~config,
               ~converter=objConverter,
               ~indent,
               ~nameGen,
               ~toJS,
               ~useCreateBucklescriptBlock,
               ~variantTables,
             );
        } else {
          value
          |> apply(
               ~config,
               ~converter=objConverter,
               ~indent,
               ~nameGen,
               ~toJS,
               ~useCreateBucklescriptBlock,
               ~variantTables,
             )
          |> Runtime.emitVariantWithPayload(
               ~label=case.label,
               ~numArgs,
               ~polymorphic=variantC.polymorphic,
               ~useCreateBucklescriptBlock,
             );
        };
      variantC.noPayloads == [] ?
        casesWithPayload(~indent) :
        EmitText.ifThenElse(
          ~indent,
          (~indent as _) => value |> EmitText.typeOfObject,
          casesWithPayload,
          (~indent as _) => value |> accessTable,
        );

    | [_, ..._] =>
      let convertCaseWithPayload = (~indent, ~numArgs, ~objConverter, case) =>
        value
        |> (
          toJS ?
            Runtime.emitVariantGetPayload(
              ~numArgs,
              ~polymorphic=variantC.polymorphic,
            ) :
            Runtime.emitJSVariantGetPayload
        )
        |> apply(
             ~config,
             ~converter=objConverter,
             ~indent,
             ~nameGen,
             ~toJS,
             ~useCreateBucklescriptBlock,
             ~variantTables,
           )
        |> (
          toJS ?
            Runtime.emitJSVariantWithPayload(
              ~label=case.labelJS |> labelJSToString,
            ) :
            Runtime.emitVariantWithPayload(
              ~label=case.label,
              ~numArgs,
              ~polymorphic=variantC.polymorphic,
              ~useCreateBucklescriptBlock,
            )
        );
      let switchCases = (~indent) =>
        variantC.withPayload
        |> List.map(((case, numArgs, objConverter)) =>
             (
               toJS ?
                 case.label
                 |> Runtime.emitVariantLabel(
                      ~polymorphic=variantC.polymorphic,
                    ) :
                 case.labelJS |> labelJSToString,
               case
               |> convertCaseWithPayload(~indent, ~numArgs, ~objConverter),
             )
           );
      let casesWithPayload = (~indent) =>
        value
        |> Runtime.(
             toJS ?
               emitVariantGetLabel(~polymorphic=variantC.polymorphic) :
               emitJSVariantGetLabel
           )
        |> EmitText.switch_(~indent, ~cases=switchCases(~indent));
      variantC.noPayloads == [] ?
        casesWithPayload(~indent) :
        EmitText.ifThenElse(
          ~indent,
          (~indent as _) => value |> EmitText.typeOfObject,
          casesWithPayload,
          (~indent as _) => value |> accessTable,
        );
    };
  };

let toJS =
    (
      ~config,
      ~converter,
      ~indent,
      ~nameGen,
      ~useCreateBucklescriptBlock,
      ~variantTables,
      value,
    ) =>
  value
  |> apply(
       ~config,
       ~converter,
       ~indent,
       ~nameGen,
       ~variantTables,
       ~toJS=true,
       ~useCreateBucklescriptBlock,
     );

let toReason =
    (
      ~config,
      ~converter,
      ~indent,
      ~nameGen,
      ~useCreateBucklescriptBlock,
      ~variantTables,
      value,
    ) =>
  value
  |> apply(
       ~config,
       ~converter,
       ~indent,
       ~nameGen,
       ~toJS=false,
       ~useCreateBucklescriptBlock,
       ~variantTables,
     );