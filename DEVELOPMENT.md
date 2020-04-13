# Development

## Build reanalyze

```
# Installs all dependencies (if needed) and builds reanalyze
npm install
npx esy install
npm run build

# If you want to run reanalyze without a complicated path
npx esy x reanalyze.exe --version
```

This will create a symlink
`_esy/default/build/install/default/bin/reanalyze.exe` which is pointing to the
executable BuckleScript picks up automatically when installed via npm.

**Note:** There is also a file called `examples/reanalyze.exe`, which is actually
also a symlink to the built reanalyze binary. It is used for our integration
tests.

## Test reanalyze

Make sure to always run the tests before submitting any changes (CI usually takes
longer to give you feedback).

```
npm test
```

## Automated Releases (for Maintainers)

The project is compatible with the [`npm
version`](https://docs.npmjs.com/cli/version) workflow. After using the `npm
version [major|minor|patch|...]` command, npm will automatically tag the
current commit, bump all the necessary version numbers (also the number in
`src/Version.re`) and push it to the current remote branch.

When a tag is pushed, [Azure
Pipelines](https://dev.azure.com/ccrisccris/reanalyze/_build) will create a
package to the Github releases
[releases](https://github.com/cristianoc/reanalyze/releases) page, which can
later be released to npm.

This CI process only covers the newest `reanalyze` releases (v3+), which are based
on BuckleScript 6.x. This version is based on OCaml 4.06 and is not compatible with
any BuckleScript version below 6.x (those are based on OCaml 4.02).

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

The releasing mechanism downloads the platform dependent artifacts stored on
the Github releases tab, so make sure to first do an automated release as
stated above (`npm version ...`).

After the CIs are done releasing the built binaries, do following command on a
unix-like system (no Windows supported):

```
node scripts/download_dist.js
```

This will download the prepared npm package from the reanalyze Github releases
tab to the root of your project with the name `reanalyze-$version.tgz`. This is
a `tgz` file ready to be released to npm!

```
# Dry run for testing
npm publish reanalyze-*.tgz --dry-run

# Publish package as @latest
npm publish reanalyze-*.tgz

# Publish package with @beta tag
npm publish reanalyze-*.tgz --tag beta
```

Consult the [npm publish](https://docs.npmjs.com/cli/publish) documentation for more options.
In case you get an `ENEEDAUTH` error, use `npm adduser` and authenticate with your npm account first.
