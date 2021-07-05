# ReScript genType

> The latest genType docs have been migrated to the [ReScript website](https://rescript-lang.org/docs/gentype/latest/introduction).

`genType` lets you export [ReScript](https://rescript-lang.org/) values and types to use in JavaScript, and import JavaScript values and types into ReScript, idiomatically. Converter functions between the two representations are generated based on the type of the value. The converters can be generated in vanilla JavaScript, or in [TypeScript](https://www.typescriptlang.org/) / [Flow](https://flow.org/en/) for a type-safe idiomatic interface.
In particular, conversion of [ReasonReact](https://reasonml.github.io/reason-react/) components both ways is supported, with automatic generation of the wrappers.

# Project status.

See [Changes.md](Changes.md) for a complete list of features, fixes, and changes for each release.

# Requirements

`rescript` 9.1.0 or higher: use `genType` 3.45.0 or higher.

`bs-platform` 9.0.1 or higher: use `genType` 3.44.0 or higher.

`bs-platform` 9.0.0 or higher: use `genType` 3.43.0 or higher.

`bs-platform` 8.3.0 or higher: use `genType` 3.36.0 or higher.

`bs-platform` 8.2.0 or higher: use `genType` 3.31.0 or higher.

`bs-platform` 8.1.1 or higher: use `genType` 3.27.0 or higher.

`bs-platform` 8.0.0 or higher: use `genType` 3.26.0 or higher.

`bs-platform` 7.3.0 or higher: use `genType` 3.18.0 or higher.

`bs-platform` 7.2.0 or higher: use `genType` 3.13.0 or higher.

`bs-platform` 7.0.2 or higher: use `genType` 3.8.0 or higher.

`bs-platform` 7.0.0 or higher: use `genType` 3.2.0 or higher.

`bs-platform` 6.2.0 or higher: use `genType` 3.0.0 or higher.

`bs-platform` 5.2.0 or higher: use `genType` 2.40.0 or higher.

`bs-platform` 5.0.x and 5.1.x: use `genType` 2.17.0 or higher.

For earlier versions, see the older [README](https://github.com/cristianoc/genType/blob/v2.16.0/README.md).

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

For running `gentype` with ReScript via `npm` workflow, add following script in your `package.json`:

```
scripts: {
  "build": "rescript",
  "clean": "rescript clean"
}
```

> **Note:** With genType < 2.17.0 or ReScript < 5.0.0, one has to set environment variable `BS_CMT_POST_PROCESS_CMD`. See the older [README](https://github.com/cristianoc/genType/blob/v2.16.0/README.md).

With this configuration, ReScript will call `gentype` for each newly built file. You might want to clean your build artifacts before usage: `npx bsb -clean-world` (otherwise there might be cached values and no `.gen.js` files are generated).

Check out the [Examples](#examples) for detailed setups (TypeScript, Flow and Plain JavaScript).

## Adding shims (TypeScript & Flow)

Configure your shim files in your `"gentypeconfig"` in [`bsconfig.json`](examples/typescript-react-example/bsconfig.json), and add relevant `.shims.js` files in a directory which is visible by ReScript e.g. [`src/shims/`](examples/typescript-react-example/src/shims). An example shim to export ReactEvent can be found [here](examples/typescript-react-example/src/shims/ReactEvent.shim.ts).

## Testing the whole setup

Open any relevant `*.res` file and add `@genType` annotations to any bindings / values / functions to be used from JavaScript. If an annotated value uses a type, the type must be annotated too. See e.g. [Hooks.res](examples/typescript-react-example/src/Hooks.res).

Save the file and rebuild the project with ReScript. You should now see a `*.gen.tsx` (for TypeScript, or `*.gen.js` for Flow) file with the same name (e.g. `MyComponent.res` -> `MyComponent.gen.tsx`).

Any values exported from `MyComponent.res` can then be imported from JS. For example:

```js
import MyComponent from "./components/MyComponent.gen";
```

# Examples

We prepared some examples to give you an idea on how to integrate `genType` in your own project. Check out the READMEs of the listed projects.

**Please make sure to build genType before trying to build the examples.**

- [flow-react-example](examples/flow-react-example/README.md)
- [typescript-react-example](examples/typescript-react-example/README.md)
- [untyped-react-example](examples/untyped-react-example/README.md)

# Documentation

Full documentation can be found [here](https://rescript-lang.org/docs/gentype/latest/introduction).

(In case you are looking for the previous version of the docs, here is an [older version](https://github.com/rescript-association/genType/blob/be699a467800b84221a7cb448e140d8f232d7025/README.md) of this README)

# Development/Contributing

Please check out our [development instructions](DEVELOPMENT.md).
