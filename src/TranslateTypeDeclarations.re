open GenTypeCommon;

type declarationKind =
  | RecordDeclarationFromTypes(
      option(string),
      list(Types.label_declaration),
    )
  | GeneralDeclaration(Parsetree.attributes, option(Typedtree.core_type))
  | GeneralDeclarationFromTypes(
      Parsetree.attributes,
      option(Types.type_expr),
    ) /* As the above, but from Types not Typedtree */
  | VariantDeclarationFromTypes(
      option(string),
      list(Types.constructor_declaration),
    )
  | NoDeclaration;

let createExportTypeFromTypeDeclaration =
    (~nameAs, ~opaque, ~typeVars, ~optType, ~annotation, ~typeEnv, typeName)
    : CodeItem.exportFromTypeDeclaration => {
  let resolvedTypeName = typeName |> TypeEnv.addModulePath(~typeEnv);
  {
    exportType: {
      nameAs,
      opaque,
      optType,
      typeVars,
      resolvedTypeName,
    },
    annotation,
  };
};

let createCase = ((label, attributes)) =>
  switch (
    attributes |> Annotation.getAttributePayload(Annotation.tagIsGenTypeAs)
  ) {
  | Some(BoolPayload(b)) => {label, labelJS: BoolLabel(b)}
  | Some(FloatPayload(s)) => {label, labelJS: FloatLabel(s)}
  | Some(IntPayload(i)) => {label, labelJS: IntLabel(i)}
  | Some(StringPayload(asLabel)) => {label, labelJS: StringLabel(asLabel)}
  | _ => {label, labelJS: StringLabel(label)}
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
    switch (typeAttributes |> Annotation.getAttributeImportRenaming) {
    | (Some(importString), nameAs) =>
      let typeName_ = typeName;
      let nameWithModulePath = typeName_ |> TypeEnv.addModulePath(~typeEnv);
      let (typeName, asTypeName) =
        switch (nameAs) {
        | Some(asString) => (asString, Some(nameWithModulePath))
        | None => (nameWithModulePath, None)
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
        |> createExportTypeFromTypeDeclaration(
             ~nameAs=None,
             ~opaque=Some(false),
             ~typeVars=[],
             ~optType=None,
             ~annotation=Generated,
             ~typeEnv,
           );

      [{CodeItem.importTypes, exportFromTypeDeclaration}];

    | (_, nameAs) =>
      switch (optType) {
      | None => [
          {
            importTypes: [],
            exportFromTypeDeclaration:
              typeName
              |> createExportTypeFromTypeDeclaration(
                   ~nameAs,
                   ~opaque=Some(true),
                   ~typeVars,
                   ~optType=Some(mixedOrUnknown(~config)),
                   ~annotation,
                   ~typeEnv,
                 ),
          },
        ]
      | Some(someType) =>
        let opaque = annotation == GenTypeOpaque ? Some(true) : None /* None means don't know */;
        let (translation: TranslateTypeExprFromTypes.translation, type_) =
          someType |> defaultCase;
        let exportFromTypeDeclaration =
          typeName
          |> createExportTypeFromTypeDeclaration(
               ~nameAs,
               ~opaque,
               ~typeVars,
               ~optType=Some(type_),
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
      }
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
           (translation, translation.type_);
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
           let type_ =
             switch (optCoreType, translation.type_) {
             | (
                 Some({ctyp_desc: Ttyp_variant(rowFields, _, _), _}),
                 Variant(variant),
               ) =>
               let rowFieldsVariants =
                 rowFields |> TranslateCoreType.processVariant;
               let noPayloads =
                 rowFieldsVariants.noPayloads |> List.map(createCase);
               let payloads =
                 if (variant.payloads
                     |> List.length
                     == (rowFieldsVariants.payloads |> List.length)) {
                   List.combine(variant.payloads, rowFieldsVariants.payloads)
                   |> List.map(
                        (((_case, i, type_), (label, attributes, _))) => {
                        let case = (label, attributes) |> createCase;
                        (case, i, type_);
                      });
                 } else {
                   variant.payloads;
                 };

               createVariant(~noPayloads, ~payloads, ~polymorphic=true);
             | _ => translation.type_
             };
           (translation, type_);
         },
       )

  | RecordDeclarationFromTypes(nameAs, labelDeclarations) =>
    let fieldTranslations =
      labelDeclarations
      |> List.map(({Types.ld_id, ld_mutable, ld_type, ld_attributes, _}) => {
           let name =
             switch (ld_attributes |> Annotation.getAttributeRenaming) {
             | Some(s) => s
             | None => ld_id |> Ident.name
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
      |> List.map(((name, mutable_, {TranslateTypeExprFromTypes.type_, _})) => {
           let (optional, type1) =
             switch (type_) {
             | Option(type1) => (Optional, type1)
             | _ => (Mandatory, type_)
             };
           {mutable_, name, optional, type_: type1};
         });
    let optType = Some(Record(fields));
    let typeVars = TypeVars.extract(typeParams);
    let opaque = Some(annotation == GenTypeOpaque);
    [
      {
        importTypes,
        exportFromTypeDeclaration:
          typeName
          |> createExportTypeFromTypeDeclaration(
               ~nameAs,
               ~opaque,
               ~typeVars,
               ~optType,
               ~annotation,
               ~typeEnv,
             ),
      },
    ];

  | VariantDeclarationFromTypes(nameAs, constructorDeclarations) =>
    let opaque = annotation == GenTypeOpaque ? Some(true) : None /* None means don't know */;
    let recordGen = Runtime.recordGen();
    let variants =
      constructorDeclarations
      |> List.map(constructorDeclaration => {
           let constructorArgs = constructorDeclaration.Types.cd_args;
           let name = constructorDeclaration.Types.cd_id |> Ident.name;
           let attributes = constructorDeclaration.Types.cd_attributes;
           let argsTranslation =
             constructorArgs
             |> TranslateTypeExprFromTypes.translateTypeExprsFromTypes(
                  ~config,
                  ~typeEnv,
                );
           let argTypes =
             argsTranslation
             |> List.map(({TranslateTypeExprFromTypes.type_, _}) => type_);
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

           let recordValue =
             recordGen
             |> Runtime.newRecordValue(~unboxed=constructorArgs == []);
           (
             name,
             attributes,
             argTypes,
             importTypes,
             recordValue |> Runtime.recordValueToString,
           );
         });
    let (variantsNoPayload, variantsWithPayload) =
      variants |> List.partition(((_, _, argTypes, _, _)) => argTypes == []);

    let noPayloads =
      variantsNoPayload
      |> List.map(((name, attributes, _argTypes, _importTypes, recordValue)) =>
           {...(name, attributes) |> createCase, label: recordValue}
         );
    let payloads =
      variantsWithPayload
      |> List.map(((name, attributes, argTypes, _importTypes, recordValue)) => {
           let type_ =
             switch (argTypes) {
             | [type_] => type_
             | _ => Tuple(argTypes)
             };
           let numArgs = argTypes |> List.length;
           (
             {...(name, attributes) |> createCase, label: recordValue},
             numArgs,
             type_,
           );
         });

    let variantTyp =
      createVariant(~noPayloads, ~payloads, ~polymorphic=false);
    let typeVars = TypeVars.(typeParams |> extract);
    let resolvedTypeName = typeName |> TypeEnv.addModulePath(~typeEnv);

    let exportFromTypeDeclaration = {
      CodeItem.exportType: {
        nameAs,
        opaque,
        optType: Some(variantTyp),
        typeVars,
        resolvedTypeName,
      },
      annotation,
    };
    let importTypes =
      variants
      |> List.map(((_, _, _, importTypes, _)) => importTypes)
      |> List.concat;

    [{exportFromTypeDeclaration, importTypes}];

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
      {typ_id, typ_type, typ_attributes, typ_manifest, _}: Typedtree.type_declaration,
    )
    : list(CodeItem.typeDeclaration) => {
  if (Debug.translation^) {
    logItem("Translate Type Declaration %s\n", typ_id |> Ident.name);
  };
  typeEnv |> TypeEnv.newType(~name=typ_id |> Ident.name);

  let annotation = typ_attributes |> Annotation.fromAttributes;
  let typeName = Ident.name(typ_id);
  let typeParams = typ_type.type_params;
  let typeVars = TypeVars.extract(typeParams);
  let nameAs = typ_attributes |> Annotation.getAttributeRenaming;

  let declarationKind =
    switch (typ_type.type_kind) {
    | Type_record(labelDeclarations, _) =>
      RecordDeclarationFromTypes(nameAs, labelDeclarations)

    | Type_variant(constructorDeclarations) =>
      VariantDeclarationFromTypes(nameAs, constructorDeclarations)

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
    : list(CodeItem.typeDeclaration) =>
  typeDeclarations
  |> List.map(
       translateTypeDeclaration(
         ~config,
         ~outputFileRelative,
         ~resolver,
         ~typeEnv,
       ),
     )
  |> List.concat;