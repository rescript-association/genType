This is a repo with examples usages of [ReasonReact](https://github.com/reasonml/reason-react), whose docs are [here](https://reasonml.github.io/reason-react/).
Have something you don't understand? Join us on [Discord](https://discord.gg/reasonml)!

```sh
git clone https://github.com/chenglou/reason-react-example.git
cd reason-react-example
npm install
npm run build
npm run webpack
```

Then open `src/index.html` to see the links to the examples (**no server needed!**).

## Watch File Changes

The above commands works for a one-time build. To continuously build when a file changes, do:

```sh
npm start
```

Then in another tab, do:

```sh
npm run webpack
```

You can then modify whichever file in `src` and refresh the html page to see the changes.

## Build for Production

```sh
npm run build
npm run webpack:production
```

This will replace the development JS artifact for an optimized version.

**To enable dead code elimination**, change `bsconfig.json`'s `package-specs` `module` from `"commonjs"` to `"es6"`. Then re-run the above 2 commands. This will allow Webpack to remove unused code.
