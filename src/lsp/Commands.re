let shellEscape = path => Filename.quote(path);

let execFull = (~input=?, ~pwd=?, cmd) => {
  let cmd =
    if (Sys.os_type == "Win32") {
      Printf.sprintf("\"%s\"", cmd);
    } else {
      cmd;
    };
  let env =
    switch (pwd) {
    | None => Unix.environment()
    | Some(pwd) =>
      Array.map(
        item =>
          String.length(item) > 4 && String.sub(item, 0, 4) == "PWD="
            ? "PWD=" ++ pwd : item,
        Unix.environment(),
      )
    };
  let prevCwd =
    switch (pwd) {
    | None => None
    | Some(pwd) =>
      let prevCwd = Unix.getcwd();
      if (prevCwd == pwd) {
        None;
      } else {
        Unix.chdir(pwd);
        Some(prevCwd);
      };
    };
  let (cmd_out, cmd_in, cmd_err) = Unix.open_process_full(cmd, env);
  switch (prevCwd) {
  | None => ()
  | Some(prevCwd) => Unix.chdir(prevCwd)
  };

  switch (input) {
  | None => ()
  | Some(text) => output_string(cmd_in, text)
  };
  close_out(cmd_in);

  let cmd_out_descr = Unix.descr_of_in_channel(cmd_out);
  let cmd_err_descr = Unix.descr_of_in_channel(cmd_err);
  let rec loop = ((out, err, opens)) =>
    if (opens == []) {
      (out, err);
    } else {
      let (can_read, _, _) = Unix.select(opens, [], [], 1.0);
      List.fold_left(
        ((out, err, opens), fh) =>
          try(
            if (fh == cmd_err_descr) {
              (out, [input_line(cmd_err), ...err], opens);
            } else {
              ([input_line(cmd_out), ...out], err, opens);
            }
          ) {
          | End_of_file => (out, err, List.filter(fh' => fh != fh', opens))
          },
        (out, err, opens),
        can_read,
      )
      |> loop;
    };
  let (out, err) = loop(([], [], [cmd_out_descr, cmd_err_descr]));
  let out = List.rev(out);
  let err = List.rev(err);
  switch (Unix.close_process_full((cmd_out, cmd_in, cmd_err))) {
  | WEXITED(0) => (out, err, true)
  | WEXITED(_)
  | WSIGNALED(_)
  | WSTOPPED(_) => (out, err, false)
  };
};
