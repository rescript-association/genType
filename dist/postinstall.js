var path = require("path");
var fs = require("fs");
var platform = process.platform;

/* We always use the .exe extension, no matter if unix / win32 */
const targetPath = path.join(__dirname, "reanalyze.exe");

function fail(msg) {
    console.warn(msg);
    process.exit(1);
}

function getPlatformBinaryPath(platform) {
  return path.join(__dirname, "vendor-" + platform, "reanalyze.exe");
}

function movePlatformBinary(platform) {
  const sourcePath = getPlatformBinaryPath(platform);

  if(!fs.existsSync(sourcePath)) {
      return fail("error: executable not found: " + sourcePath);
  }
  fs.renameSync(sourcePath, targetPath);
  fs.chmodSync(targetPath, 0777);
}

switch (platform) {
  case "win32":
  case "linux":
  case "darwin":
    movePlatformBinary(platform);
    break;
  default:
    fail("error: no release built for the " + platform + " platform");
}
