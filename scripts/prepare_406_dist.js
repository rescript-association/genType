/*
PLEASE NOTE:
This script is intended to be used on unix-like systems and used for publishing npm packages.
*/


/*
 * This script is a temporary solution for releasing 4.06 based npm packages until we finally figure
 * out the CI parts. 
 *
 * Things missing on the CI:
 * - Build the 4_06_1 branch with esy
 * - Release the build artifacts as `gentype-4.06-macos.tar.gz` etc. on GH releases
 *
 * After this is done, we can adapt this script to download the release from GH and release it to npm.
 * Basically the same workflow as with the 4.02 version, except that we attach the `4.06` npm release tag.
*/

const child_process = require("child_process");
const pjson = require("../package.json");
const fs = require("fs");
const path = require("path");

const dist = path.join(__dirname, "..", "dist");

const publicPackageJson = version => ({
  name: pjson.name,
  bin: {
    "gentype.exe": "gentype.exe"
  },
  version: pjson.version,
  description: pjson.description,
  license: pjson.license,
  author: pjson.author,
  bugs: pjson.bugs,
  homepage: pjson.homepage
});

/*
* Example:
* Github release link `gentype-macos.tar.gz`
* should end up in the `dist/vendor-darwin` directory
*/
const vendorDir = platform => {
  if (platform === "win") {
    return "vendor-win32";
  }
  if (platform === "macos") {
    return "vendor-darwin";
  }
  if (platform === "linux") {
    return "vendor-linux";
  }
  return "vendor-" + platform;
};

const targetLink = (releasePlatform, version) =>
  `https://github.com/cristianoc/genType/releases/download/v${version}/gentype-4.06-${releasePlatform}.tar.gz`;

const logStep = msg => {
  console.log(`\n>>>> ${msg}`);
};

const logErr = msg => {
  console.error(`${msg}`);
};

// platform, as used in github releases: macos, linux, win
const downloadBinary = releasePlatform => {
  const distDir = path.join(dist, vendorDir(releasePlatform));
  const target = targetLink(releasePlatform, pjson.version);

  if (!fs.existsSync(distDir)) {
    logErr(`Error: dist directory does not exist: ${distDir}`);
    logErr(
      `I think you have a typo in your releasePlatform: "${releasePlatform}"`
    );
    process.exit(1);
  }

  logStep(`Download & untar "${target}" ...`);
  try {
    return child_process.execSync(
      `curl -L ${target} | tar xz -C ${distDir}
        `,
      { stdio: [0, 1, 2] }
    );
  } catch (e) {
    logErr(
      `Download / untaring failed... Is the binary gentype-${releasePlatform}.tar.gz actually released?`
    );
    process.exit(1);
  }
};

// Currently only supports macos based builds
const copyEsyGentypeBinary = () => {
  const distDir = path.join(dist, vendorDir("darwin"));

  if (!fs.existsSync(distDir)) {
    logErr(`Error: dist directory does not exist: ${distDir}`);
    logErr(
      `I think you have a typo in your releasePlatform: "${releasePlatform}"`
    );
    process.exit(1);
  }

  const gentypePath = path.join(__dirname, "..", "_esy/default/build/install/default/bin/gentype.native.exe");
  const targetFile = path.join(distDir, "gentype.native");

  logStep(`Copying "${gentypePath}" to ${targetFile} ...`);
  try {

    return child_process.execSync(`cp ${gentypePath} ${targetFile}`);
  } catch(e) {
    logErr(
      `Could not copy gentype.native.exe from esy build directory... did you build it via 'esy build'?`
    );
    process.exit(1);
  }
};

const downloadBinaries = () => {
  ["macos", "linux", "win"].forEach(releasePlatform =>
    downloadBinary(releasePlatform)
  );
};

const writePackageJson = () => {
  const packageJson = {
    name: pjson.name,
    bin: {
      gentype: "gentype.exe"
    },
    scripts: {
      postinstall: "node postinstall.js"
    },
    version: pjson.version + "-4.06",
    description: pjson.description + " (this build only works for ocaml version 4.06!)",
    license: pjson.license,
    author: pjson.author,
    bugs: pjson.bugs,
    homepage: pjson.homepage
  };
  const data = JSON.stringify(packageJson, null, 2);
  const packageJsonPath = path.join(__dirname, "..", "dist", "package.json");

  return fs.writeFileSync(packageJsonPath, data, "utf8");
};

const main = () => {
  // TODO: Download binaries as soon as CI releases stuff
  //downloadBinaries();
  
  copyEsyGentypeBinary();
  writePackageJson();

  console.log("Preparation successful!");
};

main();
