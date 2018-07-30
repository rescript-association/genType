# Reason genFlow

`genFlow` is a tool to automatically generate type-safe bindings between [Reason](https://reasonml.github.io/) and [Flow](https://flow.org/en/). It also generates typed JS wrappers for [ReasonReact](https://reasonml.github.io/reason-react/) components, and in the other direction, if you have ReasonReact wrappers for JS components, it checks that they are well typed.

###Work in progress, only for early adopters. It is possible that the workflow will change in future.
###

Type-safe wrappers for using ReasonReact components from javascript are generated when the annotation `[@genFLow] let make ...` is added to the component definition.

[Here is a video illustrating the conversion of a ReasonReact component.](https://youtu.be/k9QYjq0c8rA)
[![IMAGE ALT TEXT HERE](assets/genFlowInAction.png)](https://youtu.be/k9QYjq0c8rA)


When a `[@genFlow]` annotation is added to some declaration in `Module.re`, a file `Module.re.js` is generated. Value and type declarations are supported.

[Here is a video showing the generation of .re.js files.](https://youtu.be/0YLXGBiB9dY)
[![IMAGE ALT TEXT HERE](assets/genFlow.png)](https://youtu.be/0YLXGBiB9dY)

When a `[@genFlow]` annotation is added to an external `ReasonReact.reactclass` binding and to the `make` function that calls `ReasonReact.wrapJsForReason` with it, a function `checkJsWrapperType` is generated to catch type errors in wrapping the JS component.

[Here is a video showing how to safely wrap JS components for use from Reason.](https://youtu.be/UKACByHmuQE)
[![IMAGE ALT TEXT HERE](assets/genFlowWrapJsComponent.png)](https://youtu.be/UKACByHmuQE)


# Set up genFlow in a project

There are some steps to set up `genFlow` in a project.
Some of this might become simpler if `genFlow` gets integrated
into bucklescript in future.

1. Set environment variable `BS_CMT_POST_PROCESS_CMD`, as explained below, before building, or starting a watcher, or starting vscode with bsb integration.
2. Add a file [genflowconfig.json](sample-project/src/shims) in the project root, and some `.shims.js` files e.g. in a directory `shims/` just like in the [sample-project](sample-project). The `.shims.js` files must be in a source directory visible by bucklescript. See e.g. [ReactShim.shim.js](sample-project/src/shims/ReactShim.shim.js).
3. Add `[@genFlow]` annotations to the values to be used from javascript. If an annotated value uses a type, the type must be anotated too. See e.g. [Component2.re](sample-project/src/Component2.re).
4. If using webpack, set up [extension-replace-loader](https://www.npmjs.com/package/extension-replace-loader) just like in [webpack.config.js](reason-react-example/webpack.config.js).


# Limitations

* **In-source**. Currently only supports bucklescript projects with in-source generation of `.bs.js` files.

* **No iterfaces**. Currently `genFlow` does not support `[@genFlow]` annotations in interface files (`.rei`).

* **No nested modules**. Nested modules are not supported, and annotations will be ignored.

* **Limited JS wrappers**. There must be ony one `@genFlow` annotation on one external binding, and the component it binds is passed to `wrapJsForReason` (this is assumed, not checked). Also, `wrapJsForReason` in the `make` function must simply forward the props, without renaming/wrapping or modifying their values (again, this is assumed, not checked). See for example [MyBannerRe.re](reason-react-example/src/interop/MyBannerRe.re).

# Development

## Build genFlow

```
# You will need esy to install all dev dependencies
npm install -g esy

esy install

# This will build the GenFlow.exe binary
esy build
```

## Build sample-project

```
# in sample-project
export BS_CMT_POST_PROCESS_CMD="$PWD/../_build/default/src/GenFlow.exe --setProjectRoot $PWD"
yarn install
yarn build
```

Every time genFlow changes, clean and build again:

```
yarn clean
yarn build
```

## Hot reloading plus genFlow in sample-project

```
# in sample-project
export BS_CMT_POST_PROCESS_CMD="$PWD/../_build/default/src/GenFlow.exe --setProjectRoot $PWD"
yarn start
yarn serve
```

## Type check with Flow in sample-project
```
# in sample-project
flow
```

## Type-safe interop in reason-react-example

An example of type-safe interop for ReasonReact components is [reason-react-example](reason-react-example).

**Note** the webpack configuration [webpack.config.js](reason-react-example/webpack.config.js) is using [extension-replace-loader](https://www.npmjs.com/package/extension-replace-loader) to load `Module.re.js` when `Module.re` is required.

```
# in reason-react-example
export BS_CMT_POST_PROCESS_CMD="$PWD/../_build/default/src/GenFlow.exe --setProjectRoot $PWD"
yarn install
yarn start
yarn webpack
open src/index.html
```
