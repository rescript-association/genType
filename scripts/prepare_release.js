// This script will prepare the dist/ folder by adding the right meta
// data such as the package.json, LICENSE and README file

const fs = require("fs");
const path = require("path");

console.log("Creating package.json");

if (!fs.existsSync('package.json')) {
  console.error("No package.json in project root found");
  process.exit(1);
}

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
    bin: { "gentype": "gentype.exe" },
    files: [
      // Dummy file
      "gentype.exe",
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
