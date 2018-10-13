/*
 This script is used for cross-platform installing & building the example projects and
 checking the git diff of the examples directory.
 
 In a successful test scenario, after building the example projects, there should not be any changed files.
 (We check manually verified genType generated files in git, we consider diffs as regressions)
*/

const debug = require("debug")("IO");
const fs = require("fs");
const child_process = require("child_process");
const path = require("path");

const exampleDirPaths = [
  "reason-react-example",
  "typescript-react-example",
  "untyped-react-example"
].map(exampleName => path.join(__dirname, "..", "examples", exampleName));

const genTypeFile = path.join(
  __dirname,
  "..",
  "lib",
  "bs",
  "native",
  "gentype.native"
);

/*
Needed for wrapping the stdout pipe with a promise
*/
function wrappedExecFile(command, args, options) {
  return new Promise((resolve, reject) => {
    const child = child_process.execFile(
      command,
      args,
      options,
      (err, stdout, stderr) => {
        if (err) {
          debug(`${command} ${args.join(" ")} exited with ${err.code}`);
          return reject(err.code);
        }
        debug(`${command} ${args.join(" ")} exited with 0`);
        resolve(stdout);
      }
    );
  });
}

async function installExamples() {
  const tasks = exampleDirPaths.map(cwd => {
    console.log(`${cwd}: npm install (takes a while)`);
    return wrappedExecFile("npm", ["install"], { cwd });
  });

  return Promise.all(tasks);
}

async function buildExamples() {
  const tasks = exampleDirPaths.map(cwd => {
    console.log(`${cwd}: npm run build (takes a while)`);
    return wrappedExecFile("npm", ["run", "build"], { cwd });
  });

  return Promise.all(tasks);
}

async function checkDiff() {
  try {
    console.log("Checking for changes in examples/");
    await wrappedExecFile("git", ["diff-index", "HEAD", "examples"]);
  } catch (code) {
    console.error(
      "Changed files detected in path examples/! Make sure genType is emitting the right code and commit the files to git"
    );
    process.exit(1);
  }
}

async function checkSetup() {
  if (!fs.existsSync(genTypeFile)) {
      const filepath = path.relative(path.join(__dirname, ".."), genTypeFile);
      throw new Error(`${filepath} does not exist. Use \`npm run build\` first!`);
  }
}

async function main() {
  try {
    await checkSetup();
    await installExamples();
    await buildExamples();
    await checkDiff();
    console.log("Test successful!");
  } catch (e) {
    console.error(`Test failed unexpectly: ${e.message}`);
    debug(e);
  }
}

main();
