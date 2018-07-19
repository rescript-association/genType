# Reason genFlow

`genFlow` is a tool for providing type-safe bindings between Reason and Flow.

# Development

```
# You will need esy to install all dev dependencies
npm install -g esy

esy install

# This will build the GenFlow.exe binary
esy build
```

Since this project is in active development, the binary uses the
`sample-project` as a hardcoded path for testing. Before we try running
`GenFlow.exe`, we need to build the `sample-project` (which is being built
with BuckleScript):

```
cd sample-project
npm install
npm run build
```

After a successful build, you can try the binary by running
`./_build/install/default/bin/GenFlow.exe` in the project root directory.
