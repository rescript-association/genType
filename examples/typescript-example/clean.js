const glob = require("glob");
const fs = require("fs");

glob.glob("src/**/*.bs.js", function(er, files) {
  files.forEach(fileBsJs => {
    const fileTsx = fileBsJs.substring(0, fileBsJs.length - 6) + ".tsx";
    fs.unlink(fileTsx, err => {return});
  });
});
