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

Since this project is in active development, the binary uses `sample-project` as a hardcoded path for testing.

## Build the sample js project

```
# in sample-project
yarn install
yarn build
```

Every time genFlow changes, clean and build again:
```
yarn clean
yarn build
```

## Run genFlow on the sample js project

```
# in sample-project
yarn genFlow
```

## Type check with Flow
```
# in sample-project
flow
```
