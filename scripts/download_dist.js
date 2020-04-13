/*
PLEASE NOTE:
This script is intended to be used on unix-like systems and used for publishing npm packages.
*/

const child_process = require("child_process");
const pjson = require("../package.json");
const path = require("path");

const projectRoot = path.join(__dirname, "..");

const filename = `reanalyze-${pjson.version}.tgz`;

const targetLink = (version) =>
  `https://github.com/cristianoc/reanalyze/releases/download/v${version}/${filename}`;

const target = targetLink(pjson.version);

console.log(`Download release: "${target}" ...`);
try {
  child_process.execSync(
    `curl -f -O -L ${target}`,
    { stdio: [0, 1, 2] }
  );
  console.log(`File downloaded to ${projectRoot}/${filename}`);
  console.log("------------");
  console.log(`Run \`npm publish ${filename} --dry-run\` before release!`);
  console.log("------------");
} catch (e) {
  console.error(
    `Download failed... Is the package ${filename} actually released?`
  );
  console.error(e);
  process.exit(1);
}
