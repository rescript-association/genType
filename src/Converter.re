open GenTypeCommon;

type t =
  | ArrayC(t)
  | CircularC(string, t)
  | FunctionC(functionC)
  | IdentC
  | NullableC(t)
  | ObjectC(fieldsC)
  | OptionC(t)
  | PromiseC(t)
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
  functionName: option(string),
  retConverter: t,
  typeVars: list(string),
  uncurried: bool,
}
and variantC = {
  hash: int,
  noPayloads: list(case),
  withPayload: list((case, int, t)),
  polymorphic: bool,
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
  | PromiseC(c) => "promise(" ++ toString(c) ++ ")"
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

let typeGetConverterNormalized =
    (~config, ~inline, ~lookupId, ~typeNameIsInterface, type0) => {
  let circular = ref("");
  let expandOneLevel = type_ =>
    switch (type_) {
    | Ident({name}) =>
      switch (name |> lookupId) {
      | (t: CodeItem.exportTypeItem) => t.type_
      | exception Not_found => type_
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
        FunctionC({
          argConverters,
          functionName: None,
          retConverter,
          typeVars,
          uncurried,
        }),
        normalized_,
      );

    | GroupOfLabeledArgs(_) => (IdentC, None)

    | Ident({isShim, name, typeArgs}) =>
      if (visited |> StringSet.mem(name)) {
        circular := name;
        (IdentC, normalized_);
      } else {
        let visited = visited |> StringSet.add(name);
        switch (name |> lookupId) {
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
          let (converter, inlined) =
            type_ |> TypeVars.substitute(~f) |> visit(~visited);
          (converter, inline ? inlined : normalized_);
        | exception Not_found =>
          let isBaseType =
            type_ == booleanT || type_ == numberT || type_ == stringT;
          (IdentC, isShim || isBaseType || inline ? normalized_ : None);
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

    | Promise(t) =>
      let (tConverter, tNormalized) = t |> visit(~visited);
      (PromiseC(tConverter), tNormalized == None ? None : normalized_);

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
            hash: variant.hash,
            noPayloads: variant.noPayloads,
            withPayload,
            polymorphic: variant.polymorphic,
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

let typeGetConverter = (~config, ~lookupId, ~typeNameIsInterface, type_) =>
  type_
  |> typeGetConverterNormalized(
       ~config,
       ~inline=false,
       ~lookupId,
       ~typeNameIsInterface,
     )
  |> fst;

let typeGetNormalized =
    (~config, ~inline, ~lookupId, ~typeNameIsInterface, type_) =>
  type_
  |> typeGetConverterNormalized(
       ~config,
       ~inline,
       ~lookupId,
       ~typeNameIsInterface,
     )
  |> snd;

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

  | PromiseC(c) => c |> converterIsIdentity(~toJS)

  | RecordC(_) => false

  | TupleC(innerTypesC) =>
    innerTypesC |> List.for_all(converterIsIdentity(~toJS))

  | VariantC(_) => false
  };

let rec apply =
        (~config, ~converter, ~indent, ~nameGen, ~toJS, ~variantTables, value) =>
  switch (converter) {
  | _ when converter |> converterIsIdentity(~toJS) => value

  | ArrayC(c) =>
    let x = "ArrayItem" |> EmitText.name(~nameGen);
    value
    ++ ".map(function _element("
    ++ (x |> EmitType.ofTypeAny(~config))
    ++ ") { return "
    ++ (
      x
      |> apply(
           ~config,
           ~converter=c,
           ~indent,
           ~nameGen,
           ~toJS,
           ~variantTables,
         )
    )
    ++ "})";

  | CircularC(s, c) =>
    value
    |> EmitText.addComment(
         ~comment=
           "WARNING: circular type "
           ++ s
           ++ ". Only shallow converter applied.",
       )
    |> apply(~config, ~converter=c, ~indent, ~nameGen, ~toJS, ~variantTables)

  | FunctionC({
      argConverters,
      functionName,
      retConverter,
      typeVars,
      uncurried,
    }) =>
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
        |> apply(
             ~config,
             ~converter=retConverter,
             ~indent=indent2,
             ~nameGen,
             ~toJS,
             ~variantTables,
           )
      );

    let convertArg = (i, groupedArgConverter) =>
      switch (groupedArgConverter) {
      | ArgConverter(argConverter) =>
        let varName = i + 1 |> EmitText.argi(~nameGen);
        let notToJS = !toJS;
        (
          [varName],
          [
            varName
            |> apply(
                 ~config,
                 ~converter=argConverter,
                 ~indent=indent2,
                 ~nameGen,
                 ~toJS=notToJS,
                 ~variantTables,
               ),
          ],
        );
      | GroupConverter(groupConverters) =>
        let notToJS = !toJS;
        if (toJS) {
          let varName = i + 1 |> EmitText.argi(~nameGen);
          (
            [varName],
            groupConverters
            |> List.map(((label, optional, argConverter)) =>
                 varName
                 |> EmitText.fieldAccess(~label)
                 |> apply(
                      ~config,
                      ~converter=
                        optional == Optional
                        && !(argConverter |> converterIsIdentity(~toJS)) ?
                          OptionC(argConverter) : argConverter,
                      ~indent=indent2,
                      ~nameGen,
                      ~toJS=notToJS,
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
                   |> apply(
                        ~config,
                        ~converter=argConverter,
                        ~indent=indent2,
                        ~nameGen,
                        ~toJS=notToJS,
                        ~variantTables,
                      )
                 )
               )
            |> String.concat(", ");
          (varNames, ["{" ++ fieldValues ++ "}"]);
        };
      | UnitConverter =>
        let varName = i + 1 |> EmitText.argi(~nameGen);
        ([varName], [varName]);
      };

    let mkBody = bodyArgs => {
      let useCurry = !uncurried && toJS && List.length(bodyArgs) > 1;
      config.emitImportCurry = config.emitImportCurry || useCurry;
      Indent.break(~indent=indent1)
      ++ (value |> EmitText.funCall(~args=bodyArgs, ~useCurry) |> mkReturn);
    };

    let convertedArgs = argConverters |> List.mapi(convertArg);
    let args = convertedArgs |> List.map(fst) |> List.concat;
    let funParams = args |> List.map(v => v |> EmitType.ofTypeAny(~config));
    let bodyArgs = convertedArgs |> List.map(snd) |> List.concat;
    EmitText.funDef(
      ~bodyArgs,
      ~functionName,
      ~funParams,
      ~indent,
      ~mkBody,
      ~typeVars,
    );

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
             |> EmitText.fieldAccess(~label)
             |> apply(
                  ~config,
                  ~converter=fieldConverter |> simplifyFieldConverted,
                  ~indent,
                  ~nameGen,
                  ~toJS,
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
               ~variantTables,
             )
        ),
      ]);
    }

  | PromiseC(c) =>
    let x = "$promise" |> EmitText.name(~nameGen);
    value
    ++ ".then(function _element("
    ++ (x |> EmitType.ofTypeAny(~config))
    ++ ") { return "
    ++ (
      x
      |> apply(
           ~config,
           ~converter=c,
           ~indent,
           ~nameGen,
           ~toJS,
           ~variantTables,
         )
    )
    ++ "})";

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
               |> EmitText.arrayAccess(~index)
               |> apply(
                    ~config,
                    ~converter=fieldConverter |> simplifyFieldConverted,
                    ~indent,
                    ~nameGen,
                    ~toJS,
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
             |> EmitText.fieldAccess(~label)
             |> apply(
                  ~config,
                  ~converter=fieldConverter |> simplifyFieldConverted,
                  ~indent,
                  ~nameGen,
                  ~toJS,
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
           |> EmitText.arrayAccess(~index)
           |> apply(
                ~config,
                ~converter=c,
                ~indent,
                ~nameGen,
                ~toJS,
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
    if (variantC.noPayloads != []) {
      Hashtbl.replace(variantTables, (variantC.hash, toJS), variantC);
    };
    let convertToString =
      !toJS
      && variantC.noPayloads
      |> List.exists(({labelJS}) =>
           labelJS == BoolLabel(true) || labelJS == BoolLabel(false)
         ) ?
        ".toString()" : "";
    let table = variantC.hash |> variantTable(~toJS);
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
               ~variantTables,
             )
          |> Runtime.emitVariantWithPayload(
               ~config,
               ~label=case.label,
               ~numArgs,
               ~polymorphic=variantC.polymorphic,
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
             ~variantTables,
           )
        |> (
          toJS ?
            Runtime.emitJSVariantWithPayload(
              ~label=case.labelJS |> labelJSToString,
            ) :
            Runtime.emitVariantWithPayload(
              ~config,
              ~label=case.label,
              ~numArgs,
              ~polymorphic=variantC.polymorphic,
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

let toJS = (~config, ~converter, ~indent, ~nameGen, ~variantTables, value) =>
  value
  |> apply(~config, ~converter, ~indent, ~nameGen, ~variantTables, ~toJS=true);

let toReason = (~config, ~converter, ~indent, ~nameGen, ~variantTables, value) =>
  value
  |> apply(
       ~config,
       ~converter,
       ~indent,
       ~nameGen,
       ~toJS=false,
       ~variantTables,
     );