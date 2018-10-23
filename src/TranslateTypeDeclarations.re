open GenTypeCommon;

type declarationKind =
  | RecordDelaration(list(Types.label_declaration))
  | GeneralDeclaration(option(Typedtree.core_type))
  | VariantDeclaration(list(Types.constructor_declaration))
  | ImportTypeDeclaration(string, option(Annotation.attributePayload))
  | NoDeclaration;

let createExportType =
    (~opaque, ~typeVars, ~optTyp, ~typeEnv, typeName): CodeItem.t => {
  let resolvedTypeName = typeName |> TypeEnv.addModulePath(~typeEnv);
  ExportType({opaque, typeVars, resolvedTypeName, optTyp});
};

let variantLeafTypeName = (typeName, leafName) =>
  String.capitalize(typeName) ++ String.capitalize(leafName);

let translateConstructorDeclaration =
    (
      ~config as {language} as config,
      ~outputFileRelative,
      ~resolver,
      ~recordGen,
      ~genTypeKind,
      ~typeEnv,
      variantTypeName,
      constructorDeclaration,
    ) => {
  let constructorArgs = constructorDeclaration.Types.cd_args;
  let leafName = constructorDeclaration.Types.cd_id |> Ident.name;
  let leafNameResolved = leafName |> TypeEnv.addModulePath(~typeEnv);
  let argsTranslation =
    Dependencies.translateTypeExprs(~language, ~typeEnv, constructorArgs);
  let argTypes = argsTranslation |> List.map(({Dependencies.typ, _}) => typ);
  let importTypes =
    argsTranslation
    |> List.map(({Dependencies.dependencies, _}) => dependencies)
    |> List.concat
    |> List.map(
         Translation.pathToImportType(
           ~config,
           ~outputFileRelative,
           ~resolver,
         ),
       )
    |> List.concat;
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
  let codeItem =
    CodeItem.ExportVariantLeaf({
      exportType: {
        opaque: Some(true),
        typeVars,
        resolvedTypeName: variantTypeNameResolved,
        optTyp: (Some(mixedOrUnknown(~language)), genTypeKind),
      },
      constructorTyp,
      argTypes,
      leafName: leafNameResolved,
      recordValue,
    });

  (variant, {Translation.importTypes, codeItem});
};

let traslateDeclarationKind =
    (
      ~config as {language} as config,
      ~outputFileRelative,
      ~resolver,
      ~typeEnv,
      ~genTypeKind,
      ~typeName,
      ~typeVars,
      ~typeParams,
      declarationKind,
    )
    : list(Translation.declaration) =>
  switch (declarationKind) {
  | GeneralDeclaration(optCoreType) =>
    switch (optCoreType) {
    | None => [
        {
          importTypes: [],
          codeItem:
            typeName
            |> createExportType(
                 ~opaque=Some(true),
                 ~typeVars,
                 ~optTyp=(Some(mixedOrUnknown(~language)), genTypeKind),
                 ~typeEnv,
               ),
        },
      ]
    | Some(coreType) =>
      let typeExprTranslation =
        coreType.Typedtree.ctyp_type
        |> Dependencies.translateTypeExpr(~language, ~typeEnv);
      let opaque = genTypeKind == GenTypeOpaque ? Some(true) : None /* None means don't know */;
      let typ =
        switch (optCoreType, typeExprTranslation.typ) {
        | (Some({ctyp_desc: Ttyp_variant(rowFields, _, _), _}), Enum(enum))
            when rowFields |> List.length == (enum.cases |> List.length) =>
          let cases =
            List.combine(rowFields, enum.cases)
            |> List.map(((field, case)) =>
                 switch (field) {
                 | Typedtree.Ttag(label, attributes, _, _) =>
                   switch (
                     attributes
                     |> Annotation.getAttributePayload(
                          Annotation.tagIsGenTypeAs,
                        )
                   ) {
                   | Some(StringPayload(asLabel)) => {
                       label,
                       labelJS: asLabel,
                     }
                   | _ => {label, labelJS: label}
                   }
                 | Tinherit(_) => case
                 }
               );
          cases |> createEnum;
        | _ => typeExprTranslation.typ
        };
      let codeItem =
        typeName
        |> createExportType(
             ~opaque,
             ~typeVars,
             ~optTyp=(Some(typ), genTypeKind),
             ~typeEnv,
           );

      [
        {
          importTypes:
            typeExprTranslation.dependencies
            |> Translation.translateDependencies(
                 ~config,
                 ~outputFileRelative,
                 ~resolver,
               ),
          codeItem,
        },
      ];
    }

  | RecordDelaration(labelDeclarations) =>
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
             ld_type |> Dependencies.translateTypeExpr(~language, ~typeEnv),
           );
         });
    let importTypes =
      fieldTranslations
      |> List.map(((_, _, {Dependencies.dependencies, _})) => dependencies)
      |> List.concat
      |> List.map(
           Translation.pathToImportType(
             ~config,
             ~outputFileRelative,
             ~resolver,
           ),
         )
      |> List.concat;
    let fields =
      fieldTranslations
      |> List.map(((name, mutable_, {Dependencies.typ, _})) => {
           let (optional, typ1) =
             switch (typ) {
             | Option(typ1) => (Optional, typ1)
             | _ => (Mandatory, typ)
             };
           {name, optional, mutable_, typ: typ1};
         });
    let optTyp = (Some(Record(fields)), genTypeKind);
    let typeVars = TypeVars.extract(typeParams);
    let opaque = Some(genTypeKind == GenTypeOpaque);
    [
      {
        importTypes,
        codeItem:
          typeName |> createExportType(~opaque, ~typeVars, ~optTyp, ~typeEnv),
      },
    ];

  | VariantDeclaration(constructorDeclarations) =>
    let leafTypesDepsAndVariantLeafBindings = {
      let recordGen = Runtime.recordGen();
      constructorDeclarations
      |> List.map(constructorDeclaration =>
           translateConstructorDeclaration(
             ~config,
             ~outputFileRelative,
             ~resolver,
             ~recordGen,
             ~genTypeKind,
             ~typeEnv,
             typeName,
             constructorDeclaration,
           )
         );
    };
    let (variants, declarations) =
      leafTypesDepsAndVariantLeafBindings |> List.split;
    let typeParams = TypeVars.(typeParams |> extract |> toTyp);
    let variantTypeNameResolved = typeName |> TypeEnv.addModulePath(~typeEnv);
    let unionType =
      CodeItem.ExportVariantType({
        typeParams,
        variants,
        name: variantTypeNameResolved,
      });
    declarations @ [{codeItem: unionType, importTypes: []}];

  | ImportTypeDeclaration(importString, genTypeAsPayload) =>
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
        Translation.typeName,
        asTypeName,
        importPath: importString |> ImportPath.fromStringUnsafe,
        cmtFile: None,
      },
    ];
    let codeItem =
      /* Make the imported type usable from other modules by exporting it too. */
      typeName_
      |> createExportType(
           ~opaque=Some(false),
           ~typeVars=[],
           ~optTyp=(None, Generated),
           ~typeEnv,
         );

    [{importTypes, codeItem}];

  | NoDeclaration => []
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
      ~genTypeKind: genTypeKind,
      dec: Typedtree.type_declaration,
    )
    : list(Translation.declaration) => {
  typeEnv |> TypeEnv.newType(~name=dec.typ_id |> Ident.name);
  let typeName = Ident.name(dec.typ_id);
  let typeParams = dec.typ_type.type_params;
  let typeVars = TypeVars.extract(typeParams);

  let declarationKind =
    switch (dec.typ_type.type_kind, genTypeKind) {
    | (Type_record(labelDeclarations, _), GenType | GenTypeOpaque) =>
      RecordDelaration(labelDeclarations)

    | (Type_variant(constructorDeclarations), GenType)
        when !hasSomeGADTLeaf(constructorDeclarations) =>
      VariantDeclaration(constructorDeclarations)

    | (Type_abstract, _) =>
      switch (
        dec.typ_attributes
        |> Annotation.getAttributePayload(Annotation.tagIsGenTypeImport)
      ) {
      | Some(StringPayload(importString)) when typeParams == [] =>
        ImportTypeDeclaration(
          importString,
          dec.typ_attributes
          |> Annotation.getAttributePayload(Annotation.tagIsGenTypeAs),
        )

      | _ when genTypeKind != NoGenType =>
        GeneralDeclaration(dec.typ_manifest)

      | _ => NoDeclaration
      }

    | _ => NoDeclaration
    };

  declarationKind
  |> traslateDeclarationKind(
       ~config,
       ~outputFileRelative,
       ~resolver,
       ~typeEnv,
       ~genTypeKind,
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
    : list(Translation.declaration) => {
  let genTypeKind =
    switch (typeDeclarations) {
    | [dec, ..._] => dec.typ_attributes |> Annotation.getGenTypeKind
    | [] => NoGenType
    };
  typeDeclarations
  |> List.map(
       translateTypeDeclaration(
         ~config,
         ~outputFileRelative,
         ~resolver,
         ~typeEnv,
         ~genTypeKind,
       ),
     )
  |> List.concat;
};