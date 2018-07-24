# Reason genFlow

`genFlow` is a tool for providing type-safe bindings between Reason and Flow.

# Development

## Build genFlow

```
# You will need esy to install all dev dependencies
npm install -g esy

esy install

# This will build the GenFlow.exe binary
esy build
```

## Build the sample js project

```
# in sample-project
export BS_CMT_POST_PROCESS_CMD="$PWD/../_build/default/yarn install
yarn build
```

Every time genFlow changes, clean and build again:

```
yarn clean
yarn build
```

## Hot reloading plus genFlow

```
# in sample-project
export BS_CMT_POST_PROCESS_CMD="$PWD/../_build/default/src/GenFlow.exe --setProjectRoot $PWD"
yarn start
yarn serve
```

## Type check with Flow
```
# in sample-project
flow
```
