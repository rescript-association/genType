# Reason genType

[![CircleCI](https://circleci.com/gh/cristianoc/genType/tree/master.svg?style=svg)](https://circleci.com/gh/cristianoc/genType/tree/master) [![Appveyor](https://ci.appveyor.com/api/projects/status/github/cristianoc/gentype?svg=true)](https://ci.appveyor.com/project/cristianoc/gentype)

`genType` lets you export [Reason](https://reasonml.github.io/) values and types to use in JavaScript, and import JavaScript values and types into Reason, idiomatically. Converter functions between the two representations are generated based on the type of the value. The converters can be generated in vanilla JavaScript, or in [TypeScript](https://www.typescriptlang.org/) / [Flow](https://flow.org/en/) for a type-safe idiomatic interface.
In particular, conversion of [ReasonReact](https://reasonml.github.io/reason-react/) components both ways is supported, with automatic generation of the wrappers.

Here's an article describing how to use `genType` as part of a migration strategy where a tree of components is gradually converted to Reason bottom-up: [Adopting Reason: strategies, dual sources of truth, and why genType is a big deal](https://medium.com/p/c514265b466d).

The implementation of [@genType] performs a type-directed transformation of Reason programs after [bucklescript](https://github.com/BuckleScript/bucklescript) compilation. The transformed programs operate on data types idiomatic to JS. For example, a Reason function operating on a Reason record `{x:3}` (which is represented as `[3]` at runtime) is exported to a JS function operating on the corresponding JS object `{x:3}`.

The output of `genType` can be configured by using one of 3 back-ends: `untyped` to generate wrappers in vanilla JS, `typescript` to generate [TypeScript](https://www.typescriptlang.org/), and `flow` to generate JS with [Flow](https://flow.org/en/) type annotations.


[Here is a video illustrating the conversion of a ReasonReact component.](https://youtu.be/EV12EbxCPjM)
[![IMAGE ALT TEXT HERE](assets/genTypeDemo.png)](https://youtu.be/EV12EbxCPjM)


# Project status.
See [Changes.md](Changes.md) for a complete list of features, fixes, and changes for each release.

> **Disclaimer:** While most of the feature set is complete, the project is still growing and changing based on feedback. It is possible that the workflow will change in future.

# Download genType from Prebuilt Releases

```
# Will download and automatically untar the file in the current directory as gentype.native
# Make sure to put in the right version number in the `v0.X.X` placeholders (you can find the relased versions in our
# Github releases section)

# MacOS
curl -L https://github.com/cristianoc/genType/releases/download/v0.X.X/gentype-macos.tar.gz | tar xz

# Linux
curl -L https://github.com/cristianoc/genType/releases/download/v0.X.X/gentype-linux.tar.gz | tar xz
```

# Quick Start: Set up genType in existing TypeScript / Flow / BuckleScript project

There are some steps to set up `genType` in a project.
Some of this might become simpler if `genType` gets integrated
into bucklescript in future. The current requirement is `bs-platform 4.0.5` or later.

1. Build the gentype.native binary (`$GENTYPE_REPO/lib/bs/native/gentype.native`) or retrieve it from our prebuilt releases
2. Set environment variable with `export BS_CMT_POST_PROCESS_CMD="$GENTYPE_REPO/lib/bs/native/gentype.native`, before building a project, or starting a watcher / vscode with bsb integration.
3. Add a `"gentypeconfig"` option in [`bsconfig.json`](examples/typescript-react-example/bsconfig.json), and relevant `.shims.js` files in a directory which is visible by bucklescript e.g. [`src/shims/`](examples/typescript-react-example/src/shims). An example shim to export ReactEvent can be found [here](examples/typescript-react-example/src/shims/ReactEvent.shim.ts).
4. Open your relevant `*.re` file and add `[@genType]` annotations to any bindings / values / functions to be used from JavaScript. If an annotated value uses a type, the type must be annotated too. See e.g. [Component1.re](examples/reason-react-example/src/basics/Component1.re).
5. If using webpack and Flow, set up [extension-replace-loader](https://www.npmjs.com/package/extension-replace-loader) so webpack will pick up the appropriate `Foo.re.js` instead of `Foo.re` [example webpack.config.js](examples/reason-react-example/webpack.config.js).

# genType Configuration

Every `genType` powered project requires a configuration item `"gentypeconfig"` at top level in the project's `bsconfig.json`. (The use of a separate file `gentypeconfig.json` is being deprecated). The option has following structure:

```ts
...
  "gentypeconfig": {
    "language": "typescript" | "flow" | "untyped",
    "shims": {
      "ReasonReact": "ReactShim"
    }
  }
```

- **language**

  - "typescript" : Generate `*.tsx` files written in TypeScript.
  - "flow": Generate `*.re.js` files with Flow type annotations.
  - "untyped": Generate `*.re.js` files in vanilla JavaScript.

- **shims**
  - e.g. `Array<string>` with format: `"ReasonModule=JavaScriptModule"`
  - Required to export certain basic Reason data types to JS when one cannot modify the sources to add annotations (e.g. exporting Reason lists)

# Types Supported

### int

Reason values e.g. `1`, `2`, `3` are unchanged. So they are exported to JS values of type `number`.

### float

Reason values e.g. `1.0`, `2.0`, `3.0` are unchanged. So they are exported to JS values of type `number`.

### string

Reason values e.g. `"a"`, `"b"`, `"c"` are unchanged. So they are exported to JS values of type `string`.

### optionals

Reason values of type e.g. `option(int)`, such as `None`, `Some(0)`, `Some(1)`, `Some(2)`, are exported to JS values `null`, `undefined`, `0`, `1`, `2`.
The JS values are unboxed, and `null`/`undefined` are conflated.
So the option type is exported to JS type `null` or `undefined` or `number`.

### nullables

Reason values of type e.g. `Js.Nullable.t(int)`, such as `Js.Nullable.null`, `Js.Nullable.undefined`, `Js.Nullable.return(0)`, `Js.Nullable.return(1)`, `Js.Nullable.return(2)`, are exported to JS values `null`, `undefined`, `0`, `1`, `2`.
The JS values are identical: there is no conversion unless the argument type needs conversion.

### records

Reason record values of type e.g. `{x:int}` such as `{x:0}`, `{x:1}`, `{x:2}`, are exported to JS object values `{x:0}`, `{x:1}`, `{x:2}`. This requires a change of runtime representation from arrays to objects.
So they are exported to JS values of type `{x:number}`.

Since records are immutable by default, their fields will be exported to readonly property types in Flow/TS. Mutable fields are specified in Reason by e.g. `{mutable mutableField: string}`.

The `@genType.as` annotation can be used to change the name of a field on the JS side of things. So e.g. `{[@genType.as "y"] x:int}` is exported as JS type `{y:int}`.

If one field of the Reason record has option type, this is exported to an optional JS field. So for example Reason type `{x: option(int)}` is exported as JS type `{x?: number}`.

### objects

Reason object values of type e.g. `{. "x":int}` such as `{"x": 0}`, `{"x": 1}`, `{"x": 2}`, are exported as identical JS object values `{x:0}`, `{x:1}`, `{x:2}`. This requires no conversion. So they are exported to JS values of type `{x:number}`.
A conversion is required only when the type of some field requires conversions.

Since objects are immutable by default, their fields will be exported to readonly property types in Flow/TS. Mutable fields are specified in Reason by e.g. `{. [@bs.set] "mutableField": string }`.

It is possible to mix object and option types, so for example the Reason type `{. "x":int, "y":option(string)}` exports to JS type `{x:number, ?y: string}`, requires no conversion, and allows option pattern matching on the Reason side.

### tuples

Reason tuple values of type e.g. `(int, string)` are exported as identical JS values of type `[number, string]`. This requires no conversion, unless one of types of the tuple items does.
While the type of Reason tuples is immutable, there's currently no mature enforcement in TS/Flow, so they're currenty exported to mutable tuples.

### variants

Reason values of variant type e.g. `| A | B(int)` have the same representation when exported to JS. Constructor functions with the same name as the variants are generated, so e.g. `A` and `B(3)` are valid JS programs to generate Reason values.

### arrays

Arrays with elements of Reason type `t` are exported to JS arrays with elements of the corresponding JS type. If a conversion is required, a copy of the array is performed.

### functions

Reason functions are exported as JS functions of the corresponding type.
So for example a Reason function `foo : int => int` is exported as a JS function from numbers to numbers.

If named arguments are present in the Reason type, they are grouped and exported as JS objects. For example `foo : (~x:int, ~y:int) => int` is exported as a JS function from objects of type `{x:number, y:number}` to numbers.

In case of mixed named and unnamed arguments, consecutive named arguments form separate groups. So e.g. `foo : (int, ~x:int, ~y:int, int, ~z:int) => int` is exported to a JS function of type `(number, {x:number, y:number}, number, {z:number}) => number`.

### components

ReasonReact components with props of Reason types `t1`, `t2`, `t3` are exported as reactjs components with props of the JS types corresponding to `t1`, `t2`, `t3`. The annotation is on the `make` function: `[@genType] let make ...`.

A file can export many components by defining them in sub-modules. The toplevel component is also exported as default.

### enums

Enums are Reason polymorphic variants without payload: essentially flat sequences of identifiers. E.g. type `` [ | `monday | `tuesday ] ``.
The corresponding JS representation is `"monday"`, `"tuesday"`.

The `@genType.as` annotation can be used to change the name of an element on the JS side of things. So e.g. `` [ | [@genType.as "type"] `type_ ] `` exports Reason value `` `type_ `` to JS value `"type"`.

See for example [Enums.re](examples/typescript-react-example/src/Enums.re).

### imported types

It's possible to import an existing TS/Flow type as an opaque type in Reason. For example,

```reason
[@genType.import "./SomeFlowTypes"] type weekday;
```

defines a type which maps to `weekday` in `SomeFlowTypes.js`.
See for example [Types.re](examples/reason-react-example/src/basics/Types.re) and [SomeFlowTypes.js](examples/reason-react-example/src/basics/SomeFlowTypes.js).

### recursive types

Recursive types which do not require a conversion are fully supported.
If a recursive type requires a conversion, only a shallow conversion is performed, and a warning comment is included in the output. (The alternative would be to perform an expensive conversion down a data structure of arbitrary size).
See for example [Types.re](examples/typescript-react-example/src/nested/Types.re).

### polymorphic types

If a Reason type contains a type variable, the corresponding value is not converted. In other words, the conversion is the identity function. For example, a Reason function of type `{payload: 'a} => 'a` must treat the value of the payload as a black box, as a consequence of parametric polymorphism. If a typed back-end is used, the reason type is converted to the corresponding generic type.

#### exporting values from polymorphic types with hidden type variables

For cases when a value that contains a hidden type variable needs to be converted, a function can be used to produce the appropriate output:

**Doesn't work**

```reason
[@genType]
let none = None;
```

```js
export const none: ?T1 = OptionBS.none; // Errors out as T1 is not defined
```

**Works**

```reason
[@genType]
let none = () => None;
```

```js
const none = <T1>(a: T1): ?T1 => OptionBS.none;
```

# Limitations

- **BuckleScript in-source = true**. Currently only supports bucklescript projects with [in-source generation](https://bucklescript.github.io/docs/en/build-configuration#package-specs) and `.bs.js` file suffix.

# Development

## Build genType

```
npm install
npm run build
```

This will create the binary `lib/bs/native/gentype.native`, which is the executable that BuckleScript is supposed to use via `BS_CMT_POST_PROCESS_CMD`.

## Examples

We prepared some examples to give you an idea on how to integrate `genType` in your own project. Check out the README of the listed projects.

**Please make sure to build genType before trying to build the examples.**

- [reason-react-example](examples/reason-react-example/README.md)
- [typescript-react-example](examples/typescript-react-example/README.md)
- [untyped-react-example](examples/untyped-react-example/README.md)

## Manual Release Procedure for MacOS and Linux Binaries

All releases need to pass our integration test suite. We use `npm test` to run the tests, make sure this command passes to be able to build a release.

You can create `lib/gentype-macos.tar.gz` and `lib/gentype-linux.tar.gz` via our manual release procedure on a Mac. The linux binaries are created using a docker container.

```
./create-release.sh
```

**Important:**

We use [CircleCI](https://circleci.com/gh/cristianoc/genType) and [Appveyor](https://ci.appveyor.com/project/ryyppy/gentype) to build and automatically release to Github. Your manually released binaries might be overwritten by the built artifacts from tagged triggered commit.

## Automated Releases (recommended - Maintainers only)

We set up the project so it is compatible with the [`npm version`](https://docs.npmjs.com/cli/version) workflow. After using the `npm version [major|minor|patch|...]` command, npm will automatically tag the current commit, bump all the necessary version numbers (also the number in `src/Version.re`) and push it to the current remote branch.

After a tag is pushed, our [CircleCI](https://circleci.com/gh/cristianoc/genType) and [Appveyor](https://ci.appveyor.com/project/cristianoc/gentype) projects will be built, tested and automatically released to the Github releases tab when all tests were successful.

**Here are the concrete commands to run:**

```
# Make sure to commit & push all current changes, the working branch should be clean
# and synced up with your remote branch

# Also make sure that your current branch is explicitly set to the relevant remote
# (`git push` instead of `git push origin master`)
git status

# For patches (0.0.X+1)
npm version patch

# For minor (0.X+1.0)
npm version minor

# For major (X+1.0.0)
npm version major
```
