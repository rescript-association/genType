const glob = require("glob");
const fs = require("fs");
const child_process = require("child_process");

glob.glob("src/**/*.bs.js", function(er, files) {
  files.forEach(fileBsJs => {
    const fileTsx = fileBsJs.substring(0, fileBsJs.length - 6) + ".gen.js";
    fs.unlink(fileTsx, err => {
      return;
    });
  });
});

const isWindows = /^win/i.test(process.platform);

child_process
  .spawn("bsb", ["-clean-world"], { stdio: "inherit", stderr: "inherit", shell: isWindows })
  .on("exit", code => process.exit(code));
