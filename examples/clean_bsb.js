/*
We need this script to consistently run BSB on Unix & Windows based systems
*/

const fs = require("fs");
const child_process = require("child_process");
const path = require("path");

const spawn = child_process.spawn;

const isWindows = /^win/i.test(process.platform);

function genTypeNativePath() {
  const base = path.join(__dirname, "..", "src", "lib", "bs", "native");
  if (isWindows) {
    return path.join(base, "gentype.native.exe");
  } else {
    return path.join(base, "gentype.native");
  }
}

const shell = isWindows ? true : false;

child_process.spawnSync(genTypeNativePath(), ["-clean"], {
  stdio: ["inherit", "inherit"],
  shell
});

child_process.spawnSync("bsb", ["-clean-world"], {
  stdio: ["inherit", "inherit"],
  shell
});

