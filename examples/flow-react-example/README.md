# flow-react-example

For the setup, run `npm install`.
Make sure to build gentype.native, as stated in the project root README.

## Running the Project

```sh

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
npm run build
npm run webpack:production
```

This will replace the development JS artifact for an optimized version.

**To enable dead code elimination**, change `bsconfig.json`'s `package-specs` `module` from `"commonjs"` to `"es6"`. Then re-run the above 2 commands. This will allow Webpack to remove unused code.
