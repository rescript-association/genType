const fs = require("fs");
const pjson = require("../package.json");
const path = require("path");

const targetFile = path.join(__dirname, "..", "src", "Version.re");

const content = `
/* CREATED BY reanalyze/scripts/bump_version_module.js */
/* DO NOT MODIFY BY HAND, WILL BE AUTOMATICALLY UPDATED BY npm version */
let version = "${pjson.version}";
`;

fs.writeFileSync(targetFile, content, "utf8");
