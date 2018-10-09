# master
- Support type Js.nullable in addition to Js.Nullable.t.
- Fix import name compare which can lead to missing type imports with the same name from diffent files.
- Add missing conversion from float to number.
- Add initial shim for Js.Array.t.
- Js.Null_undefined.t and Js.null_undefined are the same as nullable.
- Array types are not opaque.
- Shims for Obj.
- New format form shims: {"fromModuleName": "toModuleName"}.

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