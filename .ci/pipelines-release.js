/*
 * Note:
 * This file has been modified to the needs of reanalyze!
 */
const fs = require("fs");
const path = require("path");

console.log("Creating package.json");

// From the project root pwd
if (!fs.existsSync('esy.json')) {
  console.error("No esy.json in project root found");
  process.exit(1);
}

if (!fs.existsSync('package.json')) {
  console.error("No package.json in project root found");
  process.exit(1);
}

// Now require from this script's location.
const esyJson = require(path.join('..', 'esy.json'));

// We need that for the package metadata
const pjson = require(path.join('..', 'package.json'));

const packageJson = JSON.stringify(
  {
    name: pjson.name,
    version: pjson.version,
    author: pjson.author,
    bugs: pjson.bugs,
    homepage: pjson.homepage,
    license: pjson.license,
    description: pjson.description,
    repository: pjson.repository,
    scripts: {
      postinstall: "node ./postinstall.js"
    },
    bin: esyJson.esy.release.bin.reduce((acc, curr) => {
      const key = path.basename(curr, ".exe");
      return Object.assign({ [key]: curr }, acc);
    }, {}),
    files: [
      // Dummy file
      "reanalyze.exe",
      "postinstall.js",
      "vendor-linux/",
      "vendor-darwin/",
      "vendor-win32/"
    ]
  },
  null,
  2
);

fs.writeFileSync(
  path.join(__dirname, "..", "dist", "package.json"),
  packageJson,
  {
    encoding: "utf8"
  }
);

try {
  console.log("Copying LICENSE");
  fs.copyFileSync(
    path.join(__dirname, "..", "LICENSE"),
    path.join(__dirname, "..", "dist", "LICENSE")
  );
} catch (e) {
  console.warn("No LICENSE found");
}

console.log("Copying README.md");
fs.copyFileSync(
  path.join(__dirname, "..", "README.md"),
  path.join(__dirname, "..", "dist", "README.md")
);
