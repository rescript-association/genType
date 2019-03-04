open GenTypeCommon;

type declarationKind =
  | RecordDeclarationFromTypes(list(Types.label_declaration))
  | GeneralDeclaration(option(Typedtree.core_type))
  | GeneralDeclarationFromTypes(option(Types.type_expr)) /* As the above, but from Types not Typedtree */
  | VariantDeclarationFromTypes(list(Types.constructor_declaration))
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
      ~typeAttributes,
      ~typeEnv,
      ~typeName,
      ~typeVars,
      declarationKind,
    )
    : list(CodeItem.typeDeclaration) => {
  let annotation = typeAttributes |> Annotation.fromAttributes;
  let opaque = annotation == Annotation.GenTypeOpaque ? Some(true) : None /* None means don't know */;
  let (importStringOpt, nameAs) =
    typeAttributes |> Annotation.getAttributeImportRenaming;

  let handleGeneralDeclaration =
      (~translation: TranslateTypeExprFromTypes.translation) => {
    let exportFromTypeDeclaration =
      typeName
      |> createExportTypeFromTypeDeclaration(
           ~nameAs,
           ~opaque,
           ~typeVars,
           ~optType=Some(translation.type_),
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
    [{CodeItem.importTypes, exportFromTypeDeclaration}];
  };

  switch (declarationKind, importStringOpt) {
  | (_, Some(importString)) =>
    /* import type */
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

  | (GeneralDeclarationFromTypes(None) | GeneralDeclaration(None), None) => [
      {
        CodeItem.importTypes: [],
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
  | (GeneralDeclarationFromTypes(Some(typeExpr)), None) =>
    let translation =
      typeExpr
      |> TranslateTypeExprFromTypes.translateTypeExprFromTypes(
           ~config,
           ~typeEnv,
         );
    handleGeneralDeclaration(~translation);

  | (GeneralDeclaration(Some(coreType)), None) =>
    let translation =
      coreType |> TranslateCoreType.translateCoreType(~config, ~typeEnv);
    let type_ =
      switch (coreType, translation.type_) {
      | ({ctyp_desc: Ttyp_variant(rowFields, _, _), _}, Variant(variant)) =>
        let rowFieldsVariants = rowFields |> TranslateCoreType.processVariant;
        let noPayloads = rowFieldsVariants.noPayloads |> List.map(createCase);
        let payloads =
          if (variant.payloads
              |> List.length == (rowFieldsVariants.payloads |> List.length)) {
            List.combine(variant.payloads, rowFieldsVariants.payloads)
            |> List.map((((_case, i, type_), (label, attributes, _))) => {
                 let case = (label, attributes) |> createCase;
                 (case, i, type_);
               });
          } else {
            variant.payloads;
          };

        createVariant(~noPayloads, ~payloads, ~polymorphic=true);
      | _ => translation.type_
      };
    handleGeneralDeclaration(~translation={...translation, type_});

  | (RecordDeclarationFromTypes(labelDeclarations), None) =>
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

  | (VariantDeclarationFromTypes(constructorDeclarations), None) =>
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

  | (NoDeclaration, None) => []
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
      {typ_attributes, typ_id, typ_manifest, typ_params, typ_type, _}: Typedtree.type_declaration,
    )
    : list(CodeItem.typeDeclaration) => {
  if (Debug.translation^) {
    logItem("Translate Type Declaration %s\n", typ_id |> Ident.name);
  };
  typeEnv |> TypeEnv.newType(~name=typ_id |> Ident.name);

  let typeName = Ident.name(typ_id);

  let typeVars =
    typ_params
    |> List.map(((coreType, _)) => coreType)
    |> TypeVars.extractFromCoreType;

  let declarationKind =
    switch (typ_type.type_kind) {
    | Type_record(labelDeclarations, _) =>
      RecordDeclarationFromTypes(labelDeclarations)

    | Type_variant(constructorDeclarations) =>
      VariantDeclarationFromTypes(constructorDeclarations)

    | Type_abstract => GeneralDeclaration(typ_manifest)

    | _ => NoDeclaration
    };

  declarationKind
  |> traslateDeclarationKind(
       ~config,
       ~outputFileRelative,
       ~resolver,
       ~typeAttributes=typ_attributes,
       ~typeEnv,
       ~typeName,
       ~typeVars,
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