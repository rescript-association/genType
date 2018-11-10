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
