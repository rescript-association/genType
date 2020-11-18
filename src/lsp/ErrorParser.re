let parseLoc = text => {
  // pre-4.08
  let rx_pre408 =
    Str.regexp(
      {|File "[^"]*", line \([0-9]+\), characters \([0-9]+\)-\([0-9]+\):|},
    );
  // 4.08+
  let rx = Str.regexp({|Line \([0-9]+\), characters \([0-9]+\)-\([0-9]+\):|});

  if (Str.string_match(rx_pre408, text, 0)) {
    let line = Str.matched_group(1, text) |> int_of_string;
    let c0 = Str.matched_group(2, text) |> int_of_string;
    let c1 = Str.matched_group(3, text) |> int_of_string;
    Some((line - 1, c0, c1));
  } else if (Str.string_match(rx, text, 0)) {
    let line = Str.matched_group(1, text) |> int_of_string;
    let c0 = Str.matched_group(2, text) |> int_of_string;
    let c1 = Str.matched_group(3, text) |> int_of_string;
    Some((line - 1, c0, c1));
  } else {
    None;
        /* Log.log("Cannot parse type error: " ++ text); */
  };
};

let parseErrors = lines => {
  let rec loop = lines =>
    switch (lines) {
    | [] => ([], [])
    | [line, ...rest] =>
      let (tail, items) = loop(rest);
      switch (parseLoc(line)) {
      | None => ([line, ...tail], items)
      | Some(loc) => ([], [(loc, tail), ...items])
      };
    };
  let (tail, errors) = loop(lines);
  let errors = tail == [] ? errors : [((0, 0, 0), tail), ...errors];

  errors;
};

let parseDependencyError = text => {
  let rx =
    Str.regexp(
      {|Error: The files \(.+\)\.cmi
       and \(.+\)\.cmi
       make inconsistent assumptions over interface \([A-Za-z_-]+\)|},
    );

  switch (Str.search_forward(rx, text, 0)) {
  | exception Not_found => None
  | _ =>
    let dep =
      Str.matched_group(1, text)
      |> Filename.basename
      |> String.capitalize_ascii;
    let base =
      Str.matched_group(2, text)
      |> Filename.basename
      |> String.capitalize_ascii;
    let interface = Str.matched_group(3, text);
    Some((dep, base, interface));
  };
};
