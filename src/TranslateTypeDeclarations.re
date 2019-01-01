open GenTypeCommon;

type declarationKind =
  | RecordDeclarationFromTypes(list(Types.label_declaration))
  | GeneralDeclaration(Parsetree.attributes, option(Typedtree.core_type))
  | GeneralDeclarationFromTypes(
      Parsetree.attributes,
      option(Types.type_expr),
    ) /* As the above, but from Types not Typedtree */
  | VariantDeclarationFromTypes(list(Types.constructor_declaration))
  | NoDeclaration;

let createExportType =
    (~nameAs, ~opaque, ~typeVars, ~optTyp, ~annotation, ~typeEnv, typeName)
    : CodeItem.exportFromTypeDeclaration => {
  let resolvedTypeName = typeName |> TypeEnv.addModulePath(~typeEnv);
  {
    exportKind:
      ExportType({nameAs, opaque, optTyp, typeVars, resolvedTypeName}),
    annotation,
  };
};

let variantLeafTypeName = (typeName, leafName) =>
  String.capitalize(typeName) ++ String.capitalize(leafName);

let translateConstructorDeclarationFromTypes =
    (
      ~config,
      ~outputFileRelative,
      ~resolver,
      ~recordGen,
      ~typeEnv,
      variantTypeName,
      constructorDeclaration,
    ) => {
  let constructorArgs = constructorDeclaration.Types.cd_args;
  let leafName = constructorDeclaration.Types.cd_id |> Ident.name;
  let leafNameResolved = leafName |> TypeEnv.addModulePath(~typeEnv);
  let argsTranslation =
    constructorArgs
    |> TranslateTypeExprFromTypes.translateTypeExprsFromTypes(
         ~config,
         ~typeEnv,
       );
  let argTypes =
    argsTranslation |> List.map(({TranslateTypeExprFromTypes.typ, _}) => typ);
  let importTypes =
    argsTranslation
    |> List.map(({TranslateTypeExprFromTypes.dependencies, _}) =>
         dependencies
       )
    |> List.concat
    |> Translation.translateDependencies(
         ~config,
         ~outputFileRelative,
         ~resolver,
       );
  /* A valid Reason identifier that we can point UpperCase JS exports to. */

  let variantTypeNameResolved =
    variantLeafTypeName(variantTypeName, leafName)
    |> TypeEnv.addModulePath(~typeEnv);

  let typeVars = argTypes |> TypeVars.freeOfList;

  let variant = {
    name: variantTypeNameResolved,
    params: typeVars |> TypeVars.toTyp,
  };
  let constructorTyp: CodeItem.constructorTyp = {typeVars, argTypes, variant};
  let recordValue =
    recordGen |> Runtime.newRecordValue(~unboxed=constructorArgs == []);
  let exportVariantLeaf: CodeItem.exportVariantLeaf = {
    exportType: {
      nameAs: None,
      opaque: Some(true),
      optTyp: Some(mixedOrUnknown(~config)),
      typeVars,
      resolvedTypeName: variantTypeNameResolved,
    },
    constructorTyp,
    argTypes,
    leafName: leafNameResolved,
    recordValue,
  };
  (variant, (importTypes, exportVariantLeaf));
};

let traslateDeclarationKind =
    (
      ~config,
      ~outputFileRelative,
      ~resolver,
      ~typeEnv,
      ~annotation,
      ~typeName,
      ~typeVars,
      ~typeParams,
      declarationKind,
    )
    : list(CodeItem.typeDeclaration) => {
  let handleTypeAttributes = (~defaultCase, ~optType, typeAttributes) =>
    switch (
      typeAttributes
      |> Annotation.getAttributePayload(Annotation.tagIsGenTypeImport)
    ) {
    | Some(StringPayload(importString)) =>
      let genTypeAsPayload =
        typeAttributes
        |> Annotation.getAttributePayload(Annotation.tagIsGenTypeAs);
      let typeName_ = typeName;
      let nameWithModulePath = typeName_ |> TypeEnv.addModulePath(~typeEnv);
      let (typeName, asTypeName) =
        switch (genTypeAsPayload) {
        | Some(StringPayload(asString)) => (
            asString,
            Some(nameWithModulePath),
          )
        | _ => (nameWithModulePath, None)
        };
      let importTypes = [
        {
          CodeItem.typeName,
          asTypeName,
          importPath: importString |> ImportPath.fromStringUnsafe,
          cmtFile: None,
        },
      ];
      let exportFromTypeDeclaration =
        /* Make the imported type usable from other modules by exporting it too. */
        typeName_
        |> createExportType(
             ~nameAs=None,
             ~opaque=Some(false),
             ~typeVars=[],
             ~optTyp=None,
             ~annotation=Generated,
             ~typeEnv,
           );

      [{CodeItem.importTypes, exportFromTypeDeclaration}];

    | _ =>
      let nameAs =
        switch (
          typeAttributes
          |> Annotation.getAttributePayload(Annotation.tagIsGenTypeAs)
        ) {
        | Some(StringPayload(s)) => Some(s)
        | _ => None
        };
      switch (optType) {
      | None => [
          {
            importTypes: [],
            exportFromTypeDeclaration:
              typeName
              |> createExportType(
                   ~nameAs,
                   ~opaque=Some(true),
                   ~typeVars,
                   ~optTyp=Some(mixedOrUnknown(~config)),
                   ~annotation,
                   ~typeEnv,
                 ),
          },
        ]
      | Some(someType) =>
        let opaque = annotation == GenTypeOpaque ? Some(true) : None /* None means don't know */;
        let (translation: TranslateTypeExprFromTypes.translation, typ) =
          someType |> defaultCase;
        let exportFromTypeDeclaration =
          typeName
          |> createExportType(
               ~nameAs,
               ~opaque,
               ~typeVars,
               ~optTyp=Some(typ),
               ~annotation,
               ~typeEnv,
             );
        let importTypes =
          opaque == Some(true) ?
            [] :
            translation.dependencies
            |> Translation.translateDependencies(
                 ~config,
                 ~outputFileRelative,
                 ~resolver,
               );
        [{importTypes, exportFromTypeDeclaration}];
      };
    };

  switch (declarationKind) {
  | GeneralDeclarationFromTypes(typeAttributes, optTypeExpr) =>
    typeAttributes
    |> handleTypeAttributes(
         ~optType=optTypeExpr,
         ~defaultCase=typeExpr => {
           let translation =
             typeExpr
             |> TranslateTypeExprFromTypes.translateTypeExprFromTypes(
                  ~config,
                  ~typeEnv,
                );
           (translation, translation.typ);
         },
       )

  | GeneralDeclaration(typeAttributes, optCoreType) =>
    typeAttributes
    |> handleTypeAttributes(
         ~optType=optCoreType,
         ~defaultCase=coreType => {
           let translation =
             coreType
             |> TranslateCoreType.translateCoreType(~config, ~typeEnv);
           let typ =
             switch (optCoreType, translation.typ) {
             | (
                 Some({ctyp_desc: Ttyp_variant(rowFields, _, _), _}),
                 Enum({withPayload}),
               ) =>
               let {TranslateCoreType.noPayloads} =
                 rowFields |> TranslateCoreType.processVariant;
               let cases =
                 noPayloads
                 |> List.map(((label, attributes)) =>
                      switch (
                        attributes
                        |> Annotation.getAttributePayload(
                             Annotation.tagIsGenTypeAs,
                           )
                      ) {
                      | Some(BoolPayload(b)) => {
                          label,
                          labelJS: BoolLabel(b),
                        }
                      | Some(FloatPayload(s)) => {
                          label,
                          labelJS: FloatLabel(s),
                        }
                      | Some(IntPayload(i)) => {label, labelJS: IntLabel(i)}
                      | Some(StringPayload(asLabel)) => {
                          label,
                          labelJS: StringLabel(asLabel),
                        }
                      | _ => {label, labelJS: StringLabel(label)}
                      }
                    );
               cases |> createEnum(~withPayload);
             | _ => translation.typ
             };
           (translation, typ);
         },
       )

  | RecordDeclarationFromTypes(labelDeclarations) =>
    let fieldTranslations =
      labelDeclarations
      |> List.map(({Types.ld_id, ld_mutable, ld_type, ld_attributes, _}) => {
           let name =
             switch (
               ld_attributes
               |> Annotation.getAttributePayload(Annotation.tagIsGenTypeAs)
             ) {
             | Some(StringPayload(s)) => s
             | _ => ld_id |> Ident.name
             };
           let mutability = ld_mutable == Mutable ? Mutable : Immutable;
           (
             name,
             mutability,
             ld_type
             |> TranslateTypeExprFromTypes.translateTypeExprFromTypes(
                  ~config,
                  ~typeEnv,
                ),
           );
         });
    let importTypes =
      fieldTranslations
      |> List.map(((_, _, {TranslateTypeExprFromTypes.dependencies, _})) =>
           dependencies
         )
      |> List.concat
      |> Translation.translateDependencies(
           ~config,
           ~outputFileRelative,
           ~resolver,
         );

    let fields =
      fieldTranslations
      |> List.map(((name, mutable_, {TranslateTypeExprFromTypes.typ, _})) => {
           let (optional, typ1) =
             switch (typ) {
             | Option(typ1) => (Optional, typ1)
             | _ => (Mandatory, typ)
             };
           {name, optional, mutable_, typ: typ1};
         });
    let optTyp = Some(Record(fields));
    let typeVars = TypeVars.extract(typeParams);
    let opaque = Some(annotation == GenTypeOpaque);
    [
      {
        importTypes,
        exportFromTypeDeclaration:
          typeName
          |> createExportType(
               ~nameAs=None,
               ~opaque,
               ~typeVars,
               ~optTyp,
               ~annotation,
               ~typeEnv,
             ),
      },
    ];

  | VariantDeclarationFromTypes(constructorDeclarations) =>
    let leafTypesDepsAndVariantLeafBindings = {
      let recordGen = Runtime.recordGen();
      constructorDeclarations
      |> List.map(constructorDeclaration =>
           translateConstructorDeclarationFromTypes(
             ~config,
             ~outputFileRelative,
             ~resolver,
             ~recordGen,
             ~typeEnv,
             typeName,
             constructorDeclaration,
           )
         );
    };
    let (variants, importTypesAndLeaves) =
      leafTypesDepsAndVariantLeafBindings |> List.split;
    let importTypes = importTypesAndLeaves |> List.map(fst) |> List.concat;
    let leaves = importTypesAndLeaves |> List.map(snd);
    let typeVars = TypeVars.(typeParams |> extract);
    let resolvedTypeName = typeName |> TypeEnv.addModulePath(~typeEnv);
    let unionType = {
      CodeItem.exportKind:
        ExportVariantType({leaves, resolvedTypeName, typeVars, variants}),
      annotation,
    };
    [{exportFromTypeDeclaration: unionType, importTypes}];

  | NoDeclaration => []
  };
};

let hasSomeGADTLeaf = constructorDeclarations =>
  List.exists(
    declaration => declaration.Types.cd_res !== None,
    constructorDeclarations,
  );

let translateTypeDeclaration =
    (
      ~config,
      ~outputFileRelative,
      ~resolver,
      ~typeEnv,
      ~annotation: Annotation.t,
      {typ_id, typ_type, typ_attributes, typ_manifest, _}: Typedtree.type_declaration,
    )
    : list(CodeItem.typeDeclaration) => {
  if (Debug.translation^) {
    logItem("Translate Type Declaration %s\n", typ_id |> Ident.name);
  };
  typeEnv |> TypeEnv.newType(~name=typ_id |> Ident.name);
  let typeName = Ident.name(typ_id);
  let typeParams = typ_type.type_params;
  let typeVars = TypeVars.extract(typeParams);

  let declarationKind =
    switch (typ_type.type_kind) {
    | Type_record(labelDeclarations, _) =>
      RecordDeclarationFromTypes(labelDeclarations)

    | Type_variant(constructorDeclarations)
        when !hasSomeGADTLeaf(constructorDeclarations) =>
      VariantDeclarationFromTypes(constructorDeclarations)

    | Type_abstract => GeneralDeclaration(typ_attributes, typ_manifest)

    | _ => NoDeclaration
    };

  declarationKind
  |> traslateDeclarationKind(
       ~config,
       ~outputFileRelative,
       ~resolver,
       ~typeEnv,
       ~annotation,
       ~typeName,
       ~typeVars,
       ~typeParams,
     );
};

let translateTypeDeclarations =
    (
      ~config,
      ~outputFileRelative,
      ~resolver,
      ~typeEnv,
      typeDeclarations: list(Typedtree.type_declaration),
    )
    : list(CodeItem.typeDeclaration) => {
  let annotation =
    switch (typeDeclarations) {
    | [dec, ..._] => dec.typ_attributes |> Annotation.fromAttributes
    | [] => NoGenType
    };
  typeDeclarations
  |> List.map(
       translateTypeDeclaration(
         ~config,
         ~outputFileRelative,
         ~resolver,
         ~typeEnv,
         ~annotation,
       ),
     )
  |> List.concat;
};