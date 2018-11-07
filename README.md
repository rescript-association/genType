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

# Requirements

bs-platform 4.0.5 or higher

# Installation

Install the binaries via `npm`:

```
npm install --save-dev gentype

# Test running gentype
npx gentype --help
```

Add a `gentypeconfig` section to your `bsconfig.json` (See [Configuration](#configuration) for details):

```
"gentypeconfig": {
    "language": "untyped",
    "shims": {},
    "debug": {
      "all": false,
      "basic": false
    }
}
```

For running `gentype` with BuckleScript via `npm` workflow, add following script in your `package.json`:

```
scripts: {
  "bs:build": "export BS_CMT_POST_PROCESS_CMD=\"gentype\" && bsb -make-world",
  "bs:clean": "bsb -clean-world"
}
```

For running `gentype` via different mechanics (global env variable etc.), you can set `BS_CMT_POST_PROCESS_CMD` to `node_modules/.bin/gentype` as well.

With this configuration, BuckleScript will call `gentype` for each newly built file. You might want to clean your build artifacts before usage: `npx bsb -clean-world ` (otherwise there might be cached values and no `.re.js` files are generated).

Check out the [Examples](#examples) for detailed setups (TypeScript, Flow and Plain JavaScript).

## Adding shims (TypeScript & Flow)

Configure your shim files in your `"gentypeconfig"` in [`bsconfig.json`](examples/typescript-react-example/bsconfig.json), and add relevant `.shims.js` files in a directory which is visible by bucklescript e.g. [`src/shims/`](examples/typescript-react-example/src/shims). An example shim to export ReactEvent can be found [here](examples/typescript-react-example/src/shims/ReactEvent.shim.ts).

## Testing the whole setup

Open any relevant `*.re` file and add `[@genType]` annotations to any bindings / values / functions to be used from JavaScript. If an annotated value uses a type, the type must be annotated too. See e.g. [Component1.re](examples/reason-react-example/src/basics/Component1.re).

Save the file and rebuild the project with BuckleScript. You should now see a `*.re.js` file with the same name (e.g. `MyComponent.re` -> `MyComponent.re.js`).

# Examples

We prepared some examples to give you an idea on how to integrate `genType` in your own project. Check out the READMEs of the listed projects.

**Please make sure to build genType before trying to build the examples.**

- [reason-react-example](examples/reason-react-example/README.md)
- [typescript-react-example](examples/typescript-react-example/README.md)
- [untyped-react-example](examples/untyped-react-example/README.md)


# Documentation


`genType` operates on two kinds of entities: *types* and *values*.
Each can be *exported* from Reason to JS, or *imported* into Reason from JS.
The main annotation is `@genType`, which by default means *export*.

### Export and Import Types
The following exports a function type `callback` to JS:

```reason
[@genType]
type callback = ReactEvent.Mouse.t => unit;
```

To instead import a type called `complexNumber` from JS module `MyMath.ts` (or `MyMath.js`), use the `@genType.import` annotation:

```reason
[@genType.import "./MyMath"]
type complexNumber;
```
This imported type will be treated as opaque by Reason.

### Export and Import Values

To export a function `callback` to JS:

```reason
[@genType]
let callback = _ => Js.log("Clicked");
```

To import a function `realValue` from  JS module `MyMath.ts` (or `MyMath.js`):

```reason
[@genType.import "./MyMath"] /* This is the module to import from. */
[@bs.module "./WrapJsValue.regen"] /* Always the name of the current file. */
/* Name and type of the JS value to import. */
external realValue: complexNumber => float = "";

```

Because of the `external` keyword, it's clear from context that this is an import, so you can also just use `@genType` and omit `.import`.

**NOTE** The argument of `@bs.module`must always be the name of the current file (In future, with compiler support, this could be automatically generated).

### Export and Import React Components

To export a ReasonReact component to JS, and automatically generate a wrapper for it, simply annotate the `make` function:


```reason
[@genType]
let make = (~onClick: callback, _children) => {
  ...component,
  render: _ => <div onClick> "Click me"->ReasonReact.string </div>,
};
```

To import and wrap a ReactJS component for use by ReasonReact, the type of the `make` function is the only information required:


```reason
[@genType.import "./MyBanner"] /* Module with the JS component to be wrapped. */
[@bs.module "./MyBannerWrapper.regen"] /* Always the name of the current file. */
/* The make function will be automatically generated from the types below. */
external make:
  (~show: bool, ~message: option(message)=?, 'a) =>
  ReasonReact.component(
    ReasonReact.stateless,
    ReasonReact.noRetainedProps,
    ReasonReact.actionless,
  ) =
  "";
```

The type of `make` must have a named argument for each prop in the JS component. Optional props have option type. The `make` function will be generated by `genType`.

### Type Expansion and @genType.opaque
If an exported type `persons` references other types in its definition, those types are also exported by default, as long as they are defined in the same file:


```reason
type name = string;
type surname = string;
type person = {name:name, surname:surname};

[@genType]
type persons = array(person);
```

If however you wish to hide from JS the fact that `name` and `surname` are strings, you can do it with the `@genType.opaque` annotation:

```reason
[@genType.opaque]
type name = string;
[@genType.opaque]
type surname = string;

type person = {
  name,
  surname,
};

[@genType]
type persons = array(person);
```

### Renaming and @genType.as
By default, entities with a given name are exported/imported with the same name. However, you might wish to change the appearence of the name on the JS side.
For example, in the case of a Reason keyword, such as `type`:

```reason
[@genType]
type shipment = {
  date: float,
  [@genType.as "type"]
  type_: string,
};
```

Or in the case of components:

```reason
[@genType]
let make =
  (~date: float) => [@genType.as "type"] (~type_: string) => ...
```

**NOTE** For technical reasons, it is not possible to rename the first argument of a function (it will be fixed once bucklescript supports OCaml 4.0.6).

You will also need to reach out for renaming when importing a capitalized type from JS, since types in Reason cannot be capitalized:

```reason
[@genType.import "./MyMath"]
[@genType.as "ComplexNumber"]
type complexNumber;
```

## Configuration

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

## Types Supported

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

Immutable arrays are supported with the additional Reason library
[ImmutableArray.re/.rei](examples/typescript-react-example/src/ImmutableArray.rei), which currently needs to be added to your project.
The type `ImmutableArray.t(+'a)` is covariant, and is mapped to readonly array types in TS/Flow. As opposed to TS/Flow, `ImmutableArray.t` does not allow casting in either direction with normal arrays. Instead, a copy must be performed using `fromArray` and `toArray`.

### functions

Reason functions are exported as JS functions of the corresponding type.
So for example a Reason function `foo : int => int` is exported as a JS function from numbers to numbers.

If named arguments are present in the Reason type, they are grouped and exported as JS objects. For example `foo : (~x:int, ~y:int) => int` is exported as a JS function from objects of type `{x:number, y:number}` to numbers.

In case of mixed named and unnamed arguments, consecutive named arguments form separate groups. So e.g. `foo : (int, ~x:int, ~y:int, int, ~z:int) => int` is exported to a JS function of type `(number, {x:number, y:number}, number, {z:number}) => number`.


To specify how a named argument is exported to JS, use the `[@genType.as "name"]` annotation:

```reason
[@genType]
let make =
  (~date: float) => [@genType.as "type"] (~type_: string) => ...
```

**NOTE** For technical reasons, it is not possible to rename the first argument of a function (it will be fixed once bucklescript supports OCaml 4.0.6).







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

### first class modules

Reason first class modules are converted from their array Reason runtime representation to JS Object types.
For example,

```reason
module type MT = { let x: int; let y: string; };
module M = { let y = "abc"; let x = 42; };
[@genType] let firstClassModule: module MT = (module M);
```

is exported as a JS object of type

```reason
{x: number, y: string}
```

Notice how the order of elements in the exported JS object is determined by the module type `MT` and not the module implementation `M`.


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

# Development/Contributing

## Build genType

```
npm install
npm run build
```

This will create the binary `lib/bs/native/gentype.native`, which is the executable that BuckleScript is supposed to use via `BS_CMT_POST_PROCESS_CMD`.

## Automated Releases (for Maintainers)

The project is compatible with the [`npm version`](https://docs.npmjs.com/cli/version) workflow. After using the `npm version [major|minor|patch|...]` command, npm will automatically tag the current commit, bump all the necessary version numbers (also the number in `src/Version.re`) and push it to the current remote branch.

When a tag is pushed, [CircleCI](https://circleci.com/gh/cristianoc/genType) and [Appveyor](https://ci.appveyor.com/project/cristianoc/gentype) projects will be built, tested and automatically released to the Github [releases](https://github.com/cristianoc/genType/releases) tab when all tests were successful.

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

## Releasing to npm (Maintainers only)

The releasing mechanism downloads the platform dependent artifacts stored on the Github releases tab, so make sure to first do an automated release as stated above (`npm version ...`).

After the CIs are done releasing the built binaries, do following command on a unix-like system (no Windows supported):

```
node scripts/prepare_dist.js
```

This will set up a `dist/package.json` file and also download all relevant build artifacts in the appropriate `dist/vendor-*` directory. After running the preparation script successfully, you can publish dist as if it were any common npm package. Here are some examples:

```
# Dry run for testing
npm publish dist --dry-run

# Publish package as @latest
npm publish dist

# Publish package with @beta tag
npm publish dist/ --tag beta dist
```

Consult the [npm publish](https://docs.npmjs.com/cli/publish) documentation for more options.
In case you get an `ENEEDAUTH` error, use `npm adduser` and authenticate with your npm account first.


**Pro tip:** If you want to publish a dist with a different version number (e.g. for testing the publishing process), you can also manually modify the `version` number in the `dist/package.json` file before releasing.


## Manual Releases (MacOS & Linux)

All releases need to pass our integration test suite. We use `npm test` to run the tests, make sure this command passes to be able to build a release.

You can create `lib/gentype-macos.tar.gz` and `lib/gentype-linux.tar.gz` via our manual release procedure on a Mac. The linux binaries are created using a docker container.

```
./create-release.sh
```

**Important:**

We use [CircleCI](https://circleci.com/gh/cristianoc/genType) and [Appveyor](https://ci.appveyor.com/project/ryyppy/gentype) to build and automatically release to Github. Your manually released binaries might be overwritten by the built artifacts from tagged triggered commit.

