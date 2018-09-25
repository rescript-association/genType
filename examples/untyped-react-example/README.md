# reason-react-example

For the setup, run `npm install`.

## Running the Project

```sh
# Make sure to build genflow.native, as stated in the project root README
export BS_CMT_POST_PROCESS_CMD="$PWD/../../lib/bs/native/genflow.native"

# If not done yet, do an initial build
npm run build

# In one tab, run webpack
npm run webpack

# In another tab, start the BuckleScript compiler
npm start
```

Then open `src/index.html` to see the links to the examples (**no server needed!**).

## Build for Production

```sh
export BS_CMT_POST_PROCESS_CMD="$PWD/../../lib/bs/native/genflow.native"
npm run build
npm run webpack:production
```

This will replace the development JS artifact for an optimized version.

**To enable dead code elimination**, change `bsconfig.json`'s `package-specs` `module` from `"commonjs"` to `"es6"`. Then re-run the above 2 commands. This will allow Webpack to remove unused code.
