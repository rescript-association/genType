type channel =
  | Stdout
  | Logfile;
let channel = Stdout;

/* log Add and Remove and file actions to see that the cmt hook is working */
let basic = ref(false);
let codeItems = ref(false);
let config = ref(false);
let converter = ref(false);
let dependencies = ref(false);
let moduleResolution = ref(false);

let notImplemented = ref(false);

let typeEnv = ref(false);

let typeResolution = ref(false);

let all = () => {
  basic := true;
  codeItems := true;
  config := true;
  converter := true;
  dependencies := true;
  moduleResolution := true;
  notImplemented := true;
  typeEnv := true;
  typeResolution := true;
};