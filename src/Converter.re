open GenTypeCommon;

type t =
  | ArrayC(t)
  | CircularC(string, t)
  | FunctionC(functionC)
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
and functionC = {
  argConverters: list(groupedArgConverter),
  retConverter: t,
  typeVars: list(string),
  uncurried: bool,
}
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

  | FunctionC({argConverters, retConverter, uncurried}) =>
    "fn"
    ++ (uncurried ? "Uncurried" : "")
    ++ "("
    ++ (
      argConverters
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
    ++ toString(retConverter)
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

let typeToConverterNormalized =
    (
      ~config,
      ~exportTypeMap: CodeItem.exportTypeMap,
      ~exportTypeMapFromOtherFiles,
      ~typeNameIsInterface,
      type0,
    ) => {
  let circular = ref("");
  let expandOneLevel = type_ =>
    switch (type_) {
    | Ident({name: s}) =>
      switch (exportTypeMap |> StringMap.find(s)) {
      | (t: CodeItem.exportTypeItem) => t.type_
      | exception Not_found =>
        switch (exportTypeMapFromOtherFiles |> StringMap.find(s)) {
        | exception Not_found => type_
        | (t: CodeItem.exportTypeItem) => t.type_
        }
      }
    | _ => type_
    };
  let rec visit = (~visited: StringSet.t, type_) => {
    let normalized_ = Some(type_);
    switch (type_) {
    | Array(t, _) =>
      let (tConverter, tNormalized) = t |> visit(~visited);
      (ArrayC(tConverter), tNormalized == None ? None : normalized_);

    | Function({argTypes, retType, typeVars, uncurried, _}) =>
      let argConverters =
        argTypes |> List.map(typeToGroupedArgConverter(~visited));
      let (retConverter, _) = retType |> visit(~visited);
      (
        FunctionC({argConverters, retConverter, typeVars, uncurried}),
        normalized_,
      );

    | GroupOfLabeledArgs(_) => (IdentC, None)

    | Ident({isShim, name: s, typeArgs}) =>
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
        | {typeVars, type_, _} =>
          let pairs =
            try (List.combine(typeVars, typeArgs)) {
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
            type_ |> TypeVars.substitute(~f) |> visit(~visited) |> fst,
            normalized_,
          );
        | exception Not_found =>
          let isBaseType =
            type_ == booleanT || type_ == numberT || type_ == stringT;
          (IdentC, isShim || isBaseType ? normalized_ : None);
        };
      }

    | Null(t) =>
      let (tConverter, tNormalized) = t |> visit(~visited);
      (NullableC(tConverter), tNormalized == None ? None : normalized_);

    | Nullable(t) =>
      let (tConverter, tNormalized) = t |> visit(~visited);
      (NullableC(tConverter), tNormalized == None ? None : normalized_);

    | Object(_, fields) => (
        ObjectC(
          fields
          |> List.map(({name, optional, type_: t, _}) =>
               (
                 name,
                 (optional == Mandatory ? t : Option(t))
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
          |> List.map(({name, optional, type_, _}) =>
               (
                 name,
                 (optional == Mandatory ? type_ : Option(type_))
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
          let unboxed = t |> expandOneLevel |> typeIsObject;
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
  and typeToGroupedArgConverter = (~visited, type_) =>
    switch (type_) {
    | GroupOfLabeledArgs(fields) =>
      GroupConverter(
        fields
        |> List.map(({name, type_, optional, _}) =>
             (name, optional, type_ |> visit(~visited) |> fst)
           ),
      )
    | _ =>
      type_ == unitT ?
        UnitConverter : ArgConverter(type_ |> visit(~visited) |> fst)
    };

  let (converter, normalized) = type0 |> visit(~visited=StringSet.empty);
  let finalConverter =
    circular^ != "" ? CircularC(circular^, converter) : converter;
  if (Debug.converter^) {
    logItem(
      "Converter %s type0:%s converter:%s\n",
      normalized == None ? " opaque " : "",
      type0 |> EmitType.typeToString(~config, ~typeNameIsInterface),
      finalConverter |> toString,
    );
  };
  (finalConverter, normalized);
};

let typeToConverter =
    (
      ~config,
      ~exportTypeMap,
      ~exportTypeMapFromOtherFiles,
      ~typeNameIsInterface,
      type_,
    ) =>
  type_
  |> typeToConverterNormalized(
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

  | FunctionC({argConverters, retConverter, uncurried}) =>
    retConverter
    |> converterIsIdentity(~toJS)
    && (!toJS || uncurried || argConverters |> List.length <= 1)
    && argConverters
    |> List.for_all(groupedArgConverter =>
         switch (groupedArgConverter) {
         | ArgConverter(argConverter) =>
           argConverter |> converterIsIdentity(~toJS=!toJS)
         | GroupConverter(_) => false
         | UnitConverter => uncurried || toJS
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
          ~importCurry,
          ~indent,
          ~nameGen,
          ~toJS,
          ~useCreateBucklescriptBlock,
          ~variantTables,
          value: Value.t,
        ) =>
  switch (converter) {
  | _ when converter |> converterIsIdentity(~toJS) => value |> Value.toString

  | ArrayC(c) =>
    let x = "ArrayItem" |> EmitText.name(~nameGen);
    (value |> Value.toString)
    ++ ".map(function _element("
    ++ (x |> EmitType.ofTypeAnyTS(~config))
    ++ ") { return "
    ++ (
      x
      |> Value.fromString
      |> apply(
           ~config,
           ~converter=c,
           ~importCurry,
           ~indent,
           ~nameGen,
           ~toJS,
           ~useCreateBucklescriptBlock,
           ~variantTables,
         )
    )
    ++ "})";

  | CircularC(s, c) =>
    value
    |> Value.addComment(
         ~comment=
           "WARNING: circular type "
           ++ s
           ++ ". Only shallow converter applied.",
       )
    |> apply(
         ~config,
         ~converter=c,
         ~importCurry,
         ~indent,
         ~nameGen,
         ~toJS,
         ~useCreateBucklescriptBlock,
         ~variantTables,
       )

  | FunctionC({argConverters, retConverter, typeVars, uncurried}) =>
    let resultName = EmitText.resultName(~nameGen);
    let indent1 = indent |> Indent.more;
    let indent2 = indent1 |> Indent.more;

    let mkReturn = x =>
      "const "
      ++ resultName
      ++ " = "
      ++ x
      ++ ";"
      ++ Indent.break(~indent=indent1)
      ++ "return "
      ++ (
        resultName
        |> Value.fromString
        |> apply(
             ~config,
             ~converter=retConverter,
             ~importCurry,
             ~indent=indent2,
             ~nameGen,
             ~toJS,
             ~useCreateBucklescriptBlock,
             ~variantTables,
           )
      );

    let convertArg = (i, groupedArgConverter) =>
      switch (groupedArgConverter) {
      | ArgConverter(argConverter) =>
        let varName = i + 1 |> EmitText.argi(~nameGen) |> Value.fromString;
        let notToJS = !toJS;
        (
          [varName],
          [
            varName
            |> apply(
                 ~config,
                 ~converter=argConverter,
                 ~importCurry,
                 ~indent=indent2,
                 ~nameGen,
                 ~toJS=notToJS,
                 ~useCreateBucklescriptBlock,
                 ~variantTables,
               ),
          ],
        );
      | GroupConverter(groupConverters) =>
        let notToJS = !toJS;
        if (toJS) {
          let varName = i + 1 |> EmitText.argi(~nameGen) |> Value.fromString;
          (
            [varName],
            groupConverters
            |> List.map(((label, optional, argConverter)) =>
                 varName
                 |> Value.fieldAccess(~label)
                 |> apply(
                      ~config,
                      ~converter=
                        optional == Optional
                        && !(argConverter |> converterIsIdentity(~toJS)) ?
                          OptionC(argConverter) : argConverter,
                      ~importCurry,
                      ~indent=indent2,
                      ~nameGen,
                      ~toJS=notToJS,
                      ~useCreateBucklescriptBlock,
                      ~variantTables,
                    )
               ),
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
                   |> Value.fromString
                   |> apply(
                        ~config,
                        ~converter=argConverter,
                        ~indent=indent2,
                        ~importCurry,
                        ~nameGen,
                        ~toJS=notToJS,
                        ~useCreateBucklescriptBlock,
                        ~variantTables,
                      )
                 )
               )
            |> String.concat(", ");
          (
            varNames |> List.map(Value.fromString),
            ["{" ++ fieldValues ++ "}"],
          );
        };
      | UnitConverter =>
        let varName = i + 1 |> EmitText.argi(~nameGen);
        ([varName |> Value.fromString], [varName]);
      };

    let mkBody = bodyArgs => {
      let useCurry = !uncurried && toJS && List.length(bodyArgs) > 1;
      importCurry := importCurry^ || useCurry;
      Indent.break(~indent=indent1)
      ++ (
        value
        |> Value.toString
        |> EmitText.funCall(~args=bodyArgs, ~useCurry)
        |> mkReturn
      );
    };

    let convertedArgs = argConverters |> List.mapi(convertArg);
    let args = convertedArgs |> List.map(fst) |> List.concat;
    let funParams =
      args
      |> List.map(v => v |> Value.toString |> EmitType.ofTypeAnyTS(~config));
    let bodyArgs = convertedArgs |> List.map(snd) |> List.concat;
    EmitText.funDef(~bodyArgs, ~funParams, ~indent, ~mkBody, ~typeVars, "");

  | IdentC => value |> Value.toString

  | NullableC(c) =>
    EmitText.parens([
      (value |> Value.toString)
      ++ " == null ? "
      ++ (value |> Value.toString)
      ++ " : "
      ++ (
        value
        |> apply(
             ~config,
             ~converter=c,
             ~importCurry,
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
      |> List.map(((label, fieldConverter)) =>
           label
           ++ ":"
           ++ (
             value
             |> Value.fieldAccess(~label)
             |> apply(
                  ~config,
                  ~converter=fieldConverter |> simplifyFieldConverted,
                  ~importCurry,
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
    let valueNullChecked = value |> Value.nullChecked;
    if (toJS) {
      EmitText.parens([
        valueNullChecked
        ++ " == null ? "
        ++ (value |> Value.toString)
        ++ " : "
        ++ (
          value
          |> apply(
               ~config,
               ~converter=c,
               ~importCurry,
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
        valueNullChecked
        ++ " == null ? undefined : "
        ++ (
          value
          |> apply(
               ~config,
               ~converter=c,
               ~importCurry,
               ~indent,
               ~nameGen,
               ~toJS,
               ~useCreateBucklescriptBlock,
               ~variantTables,
             )
        ),
      ]);
    };

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
        |> List.mapi((index, (lbl, fieldConverter)) =>
             lbl
             ++ ":"
             ++ (
               value
               |> Value.arrayAccess(~index)
               |> apply(
                    ~config,
                    ~converter=fieldConverter |> simplifyFieldConverted,
                    ~importCurry,
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
        |> List.map(((label, fieldConverter)) =>
             value
             |> Value.fieldAccess(~label)
             |> apply(
                  ~config,
                  ~converter=fieldConverter |> simplifyFieldConverted,
                  ~importCurry,
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
      |> List.mapi((index, c) =>
           value
           |> Value.arrayAccess(~index)
           |> apply(
                ~config,
                ~converter=c,
                ~importCurry,
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
    let accessTable = v =>
      table ++ EmitText.array([(v |> Value.toString) ++ convertToString]);
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
               ~importCurry,
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
               ~importCurry,
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
          (~indent as _) => value |> Value.toString |> EmitText.typeOfObject,
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
             ~importCurry,
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
        |> Value.toString
        |> EmitText.switch_(~indent, ~cases=switchCases(~indent));
      variantC.noPayloads == [] ?
        casesWithPayload(~indent) :
        EmitText.ifThenElse(
          ~indent,
          (~indent as _) => value |> Value.toString |> EmitText.typeOfObject,
          casesWithPayload,
          (~indent as _) => value |> accessTable,
        );
    };
  };

let toJS =
    (
      ~config,
      ~converter,
      ~importCurry,
      ~indent,
      ~nameGen,
      ~useCreateBucklescriptBlock,
      ~variantTables,
      value,
    ) =>
  value
  |> Value.fromString
  |> apply(
       ~config,
       ~converter,
       ~importCurry,
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
      ~importCurry,
      ~indent,
      ~nameGen,
      ~useCreateBucklescriptBlock,
      ~variantTables,
      value,
    ) =>
  value
  |> Value.fromString
  |> apply(
       ~config,
       ~converter,
       ~importCurry,
       ~indent,
       ~nameGen,
       ~toJS=false,
       ~useCreateBucklescriptBlock,
       ~variantTables,
     );