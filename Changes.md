# master
- In flow, annotate function parameters with type any, just as for TypeScript.
  This helps typing conversion functions with nested null checks.

# 2.12.2
- Fix issue where importing a type which has no definition from other files could reset the type enironment.
  Resetting the type environment causes failures depending on the order of type arguments in https://github.com/cristianoc/genType/issues/158.

# 2.12.1
- Emit type parametes when generating polymorphic conversion functions. 

# 2.12.0
- Support type definitions in first-class module types.
- Handle correctly first-class module types with nested modules inside.
- Add support for first-class module types with type equations.
- Add support for functions with first-class module types and type equations.
- Fix support for type parameters in type declarations with constraints.
 
# 2.11.0
- Fix: don't export types defined in shims as opaque. 
- Add support for type Js.null (and Js.Null.t).
- Fix signature of ImmutableArray length and size.

# 2.10.0
- Fix: convert first-class module types when defined in other files.

# 2.8.0
- Allow misspelling "genType" as "gentype".
- Auto uncurry functions of type unit => ...
- Wrap functions exported to JS with Curry._n (arity n) unless the functions have uncurried type.

# 2.7.0
- Support exporting types containing type variables at toplevel withoug making them opaque.
- Support exporting types defined in modules with type constraints.
- Extend support of first class modules to type declarations.
- Treat GADT declarations like normal ADT declarations.

# 2.6.0
- Add support for translation of inferred object types (using ##) and open object type declarations.
- Add support for open object types.

# 2.5.2
- Add support for String.t and Js.String.t.
- Avoid generating an extra type in renamed imports.

# 2.5.1
- Add support for pairs in @genType.import to encode rename information.
Instead of writing

```reason
[@genType.import "./js/MyBanner"]
[@genType.as "TopLevelClass.MiddleLevelElements.MyBannerInternal"]
```

This can be used instead:
```reason
[@genType.import ("./js/MyBanner", "TopLevelClass.MiddleLevelElements.MyBannerInternal")]
```

# 2.4.1
- Fix issue with namespace mode: conversion was not performed for types defined in other files, as the correct .cmt files was not found.

# 2.4.0
- Restore support for namespaces, and make commonjs-react-example a namespace example.
- Add support for genType.as for the type name, to record type declarations.

# 2.3.1
- Fix issue where conversion functions for types defined in other files would be missing if the first declararation in that other file is not annotated with @genType.

# 2.3.0
- Flow mode: add eslint-disable.
- Fix missing conversion of optional arguments of components.

# 2.2.2
- Fix issue where default function arguments of some type requiring conversion were not checked for undefined.
- Add missing support for @genType.opaque in variants.

# 2.2.0
- Indent function definitions and if-then-else's.
- Indent variant declarations.
- Spacing after/before brackets in object types.
- In TypeScript, use the preferred semicolon in object types.

# 2.1.0
- In commonjs, import components simply as a `require` without ".default".

# 2.0.0
- New unified support for polymorphic variants and ordinary variants. If possible (at most one case with payload of object type), use an unboxed representation. Otherwise use `{tag: label, value: ...}`.
- This is a **Breaking Change**: ordinary variants used to be represented as opaque types with construction functions. Now, ordinary variants have the same representation as polymorphic variants.

# 1.9.0
- [Enums: support one object type, plus strings/booleans/integers](https://github.com/cristianoc/genType/pull/118).

# 1.7.0
- Add support for paths in `@genType.as` when importing components from JS. Also, simply support renaming.

# 1.6.1
- Fix missing import React when importing component in untyped back-end.

# 1.6.0
- [Fix issue with access to the .bs.js file when exporting nested component](https://github.com/cristianoc/genType/issues/104).
- Fix: [Emit enum conversion tables early, to avoid the case where they’re used before being defined](https://github.com/cristianoc/genType/issues/102).
- Add support for exporting uncurried function types.

# 1.5.0
- Hygiene: avoid variable capture for generated variable names (function arguments, return value, array items).

# 1.4.0
- Adapt bs-platform lib import path depending on the module kind specified in config.
- Add -clean command to delete all the generated files.
- Fix: translation of variant types should be the identity.
- Avoid accidental variable name capture when generating constructors functions for variants.
- TypeScript: avoid type errors when converting function types by giving arguments type `any`.

# 1.3.0
- Auto propagate annotations to and from variant type declarations (not just normal type declarations).
- Auto propagate annotations to types mentioned in exported values and components.

# 1.2.1
- Fix importing type via renaming. Need to export both the original and renamed type.

# 1.2.0.
- Translate empty object types as `{}`. This avoids pulling it a shim for `Js.t`.
- Support renaming via `[@genType.as "name"]` when exporting types.

# 1.1.0
- Emit `import` instead of `require` with ES6 modules in Flow/Untyped back-end, except when import cycles are possible (Reason importing JS values/components). [PR](https://github.com/cristianoc/genType/pull/92).
- Add support for `@genType.as` when importing values from JS. This supports renaming, importing default values, and importing nested values via a path.

# 1.0.0
- Don’t import type dependencies for opaque types.
- Add `bsconfig.json` option "generatedFileExtension" to configure the extension used for generated files.
- Change the extension of generated files to `.gen.tsx` (TS) and `.gen.js` (Flow/Untyped). [#90](https://github.com/cristianoc/genType/pull/90)  
- Remove (undocumented) `genType.importStrictLocal` and always allow imports from non-scrict Flow types.

# 0.28.0
- Export first-class modules as records. Also allows to call functors from JS.
- Sanitize name of JS variable when importing a value. (Replace "-" with "_").
- Fix renaming of named argument when importing function type.

# 0.27.1
- Make genType.importStrictLocal only affect the specific import, not the whole file.

# 0.27.0
- Fix issue where pulling in types from files in other directories was not working.
- Add support for annotation @genType.importStrictLocal to generate strict-local Flow files.

# 0.26.0
- Support importing types with type parameters.
- Prevent imported values from being used directly from JS.

# 0.25.0
- Remove deprecated CLI option `—setProjectRoot`.
- Support `@genType.as "name"` to rename how labeled arguments in functions are exported. Works for function definitions and function types declarations. In parcticular, the `make` function for components. In case of function definitions, the first argument can't be renamed because of a compiler bug. [See this issue](https://github.com/cristianoc/genType/issues/75).

# 0.24.0
- Improved debug output and number of options, and made controllable from bsconfig.json.
- Fixed issue with import strings containig `.\directory` on Windows.
- Extended support for signatures and module types.
- Module include is supported for Type Expansion.
- First-orer modules are supported for Type Expansion.
- Functor application supported for Type Expansion.
- Annotating a type means all the types mentioned in it are also considered annotated. (Implemented via Type Expansion).
- Add support for type expansion [See this issue](https://github.com/cristianoc/genType/issues/70)

# 0.23.0
- Clean up terminology used in README.md. Following the cleanup plan on import/export in https://github.com/cristianoc/genType/issues/70.
- Implement principle 1: an imported type is also exported to other modules.
- Support tuple types.
- In Flow, keep opaque types also opaque internally, so export erros are caught early like in TS.
- Fix: Type names for types which are defined somewhere are not considered opaque.
- Fix: no conversion generated for opaque types.
- Generated code can be in a different order, because of refactoring how type declarations are processed.

# 0.22.0
- Support the case where the `make` function of an exported Reason component could be curried.
- Support `ImmutableArray.t` type for convesion to `ReadonlyArray` in TS/Flow.
- Add library for immutable arrays.
- Fix missing handling of `@genType.opaque`.
- Allow importing types from within nested modules.
- Allow a combination of `@genType.import` and `@genType.as` to specify an imported type.
- Object and record fields are now mapped to readonly propeties, unless they're mutable.

# 0.21.0
- Support nested components. So it's possible to define several components in one file.
- Support for recursive types. If a recursive type requires a conversion, only a shallow one is performed, and a warning comment is emitted.
- Update README describing how genType works in both directions.

# 0.20.0
- Add support for Enum types.

# 0.19.0
- Add support for nested modules: translation of nested types and values, as well as cross references.
- [Remove deprecated way of wrapping JS components](https://github.com/cristianoc/genType/commit/9c388826c43eb9507b07fd95a05eed3ce619297d).
- [Use strict imports for wrapping JS values and components](https://github.com/cristianoc/genType/commit/81c6a7f062c87c141b734dd1bb338faf1afc8f5a).
- [Generate opaque types corresponding to an existing Flow/TS type](https://github.com/cristianoc/genType/issues/63).

# 0.18.0
- Move "gentypeconfig" inside bsconfig.json. Using a file gentypeconfig.json is deprecated.
- Remove support for deprecated @genFlow annotation, and genflowconfig.json file.

# 0.17.0
- [Experimental feature: Typed wrappers for JS values](https://github.com/cristianoc/genType/pull/60).
- [Add full support for wrapping JS component, and examples](https://github.com/cristianoc/genType/commit/50b8e87c0e8942b0de2620f9d24ba7baceb07067).

# 0.16.0
- Support type Js.nullable in addition to Js.Nullable.t.
- Fix import name compare which can lead to missing type imports with the same name from diffent files.
- Add missing conversion from float to number.
- Add initial shim for Js.Array.t.
- Js.Null_undefined.t and Js.null_undefined are the same as nullable.
- Array types are not opaque.
- Shims for Obj.
- New format form shims: {"fromModuleName": "toModuleName"}.
- Cleaner generated output, with some basic pretty-printing heuristics.
- Generate TS code compatible with --strictPropertyInitialization.

# 0.15.0
- Add configuration options to gentypeconfig.json for module, importPath, reasonReactPath, bsBlockPath.
- Print version information with -help and -version.
  
# 0.14.0
- Improve support for checking JS component used from Reason.
- Support for nullable types Js.Nullable.t.
- Avoid generation of type any: use mixed (Flow) or unknown (TS) instead.
- Add ReactEvent shims for TypeScript.
- Use type “void” instead of “typeof undefined”.
- Fix converter for options to JS in nested case.
- Simplify converter for optional record fields.
- Support nullable types whose argument requires conversion.
- Support for object types: Js.t(...).
- [Treat optional(_) fields of object types as with records (no conversion JS <-> Reason)](https://github.com/cristianoc/genType/issues/48).

# 0.13.0
- Stricter checks for FlowType.
- [FlowType] include @generated and @nolint in prelude.
- Support CommonJS export.
- Fix export function type: not opaque.
- Support nested modules in shim files.

# 0.12.0
- Rename the project to genType, the executable to gentype.native, the config to gentypeconfig.json, the log to .genTypeLog.

# 0.11.0
- [Import type definitions from other files](https://github.com/cristianoc/genFlow/issues/43).
- Allow gentypeconfig.json in addition to genflowconfig.json.

# 0.10.0
- [Add `@genType.as`/`@genFlow.as` to specify the name of the field seen from JS](https://github.com/cristianoc/genFlow/issues/37). 
- [Full support for polymorphic types (type instantiation works correctly to generate conversions)](https://github.com/cristianoc/genFlow/commit/9712a60fab3ef0f31ad074f15a4256939f9e40a6).

# 0.9.0
- [Support conversion of types with free type variables](https://github.com/cristianoc/genFlow/issues/35). 
- Depecate -setProjectRoot CLI option. The project root is now found automatically and the option ignored.

# 0.8.0
- [Add "export default ComponentName" for TypeScript](https://github.com/cristianoc/genFlow/issues/21). 
- [Fix: props are contravariant](https://github.com/cristianoc/genFlow/issues/22).
- Use @genType annotation (the @genFlow annotation is still supported for backward compatibility).

# 0.7.0
- Print log information to .genFlowLog in the project root insted of standard output.

# 0.6.0
- [Add support for interface files] (https://github.com/cristianoc/genFlow/issues/19).

# 0.5.0
- [Fix precedence when omitting option converter](https://github.com/cristianoc/genFlow/commit/ac2ad1ba278960ef906e97642d01e0e45f980c34).

# 0.4.0
- [Emit array type in a way that satisfies typescript’s linter](https://github.com/cristianoc/genFlow/commit/4e6674d35a4f85c2a98a7a8eb29367008245537c).

# 0.3.0
- [Support for polymorphic props](https://github.com/cristianoc/genFlow/issues/15).
- [Support conversion for user-defined types](https://github.com/cristianoc/genFlow/issues/16).
- [Fix converter issue with nested optionals](https://github.com/cristianoc/genFlow/commit/55e0360eaba1b22e02878ebd4dfe74e05f272601).
- [Add support for array types and conversions](https://github.com/cristianoc/genFlow/issues/17).
- [Add support for record types and conversions](https://github.com/cristianoc/genFlow/issues/18).

# 0.2.0
- Build with bsb-native.
- The set up path has changed to:
```
export BS_CMT_POST_PROCESS_CMD="$PWD/../lib/bs/native/genflow.native --setProjectRoot $PWD"
```

# 0.1.0
Initial release.
