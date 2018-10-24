/*
We need this script to consistently run BSB on Unix & Windows based systems
*/

const fs = require("fs");
const child_process = require("child_process");
const path = require("path");

const spawn = child_process.spawn;

const isWindows = /^win/i.test(process.platform);

function genTypeNativePath() {
  const base = path.join(__dirname, "..", "lib", "bs", "native");
  if (isWindows) {
    return path.join(base, "gentype.native.exe");
  } else {
    return path.join(base, "gentype.native");
  }
}

process.env.BS_CMT_POST_PROCESS_CMD = genTypeNativePath();

const input = (args = process.argv.slice(2));

spawn("bsb", input, { stdio: "inherit", stderr: "inherit" }).on(
  "exit",
  code => process.exit(code)
);
