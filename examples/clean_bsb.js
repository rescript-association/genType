/*
We need this script to consistently run BSB on Unix & Windows based systems
*/

const fs = require("fs");
const child_process = require("child_process");
const path = require("path");

const spawn = child_process.spawn;

const isWindows = /^win/i.test(process.platform);

const genTypeNativePath = path.join(__dirname, "../_esy/default/build/install/default/bin/reanalyze.exe");

const shell = isWindows ? true : false;

child_process.spawnSync(genTypeNativePath, ["-clean"], {
  stdio: ["inherit", "inherit"],
  shell
});

child_process.spawnSync("bsb", ["-clean-world"], {
  stdio: ["inherit", "inherit"],
  shell
});

