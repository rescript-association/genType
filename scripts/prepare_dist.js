/*
PLEASE NOTE:
This script is intended to be used on unix-like systems and used for publishing npm packages.
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
  `https://github.com/cristianoc/genType/releases/download/v${version}/gentype-${releasePlatform}.tar.gz`;

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

const downloadBinaries = () => {
  ["macos", "linux", "win"].forEach(releasePlatform =>
    downloadBinary(releasePlatform)
  );
};

const writePackageJson = () => {
  const packageJson = {
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
  };
  const data = JSON.stringify(packageJson, null, 2);
  const packageJsonPath = path.join(__dirname, "..", "dist", "package.json");

  return fs.writeFileSync(packageJsonPath, data, "utf8");
};

const main = () => {
  downloadBinaries();
  writePackageJson();
};

main();
