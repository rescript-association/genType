type channel =
  | Stdout
  | Logfile;
let channel = Stdout;
let all = false;

/* log Add and Remove and file actions to see that the cmt hook is working */
let basic = false || all;
let codeItems = false || all;
let config = false || all;
let converter = false || all;
let dependencies = false || all;
let moduleResolution = false || all;

let notImplemented = false || all;

let typeResolution = false || all;