open TopTypes;
open Infix;

let makeDiagnostic = (documentText, ((line, c1, c2), message)) => {
  open JsonShort;
  let text = String.concat("\n", message);
  let (l2, c22) =
    {
      let%opt lineOff = PartialParser.offsetOfLine(documentText, line);
      let off2 = lineOff + c2;
      PartialParser.offsetToPosition(documentText, off2);
    }
    |? (line, c2);
  switch (ErrorParser.parseDependencyError(text)) {
  | None =>
    Some(
      o([
        ("range", Protocol.rangeOfInts(line, c1, l2, c22)),
        ("message", s(text)),
        ("severity", i(Utils.startsWith(text, "Warning") ? 2 : 1)),
      ]),
    )
  | Some(_) =>
    Log.log("Ignoring 'inconsistent assumptions' error");
    Log.log(text);
    None;
  };
};

let getText = (state, uri) => {
  switch (MessageHandlers.maybeHash(state.documentText, uri)) {
  | None =>
    switch (Utils.parseUri(uri)) {
    | None => Error("Not a uri " ++ uri)
    | Some(src) => Files.readFileResult(src)
    }
  | Some((text, _version, _isClean)) => Ok(text)
  };
};

let runDiagnostics = (uri, state, ~package) => {
  Log.log("Running diagnostics for " ++ uri);
  let%try_consume documentText = getText(state, uri);
  let%try_consume result = State.getCompilationResult(uri, state, ~package);
  JsonShort.(
    Rpc.sendNotification(
      stdout,
      "textDocument/publishDiagnostics",
      o([
        ("uri", s(uri)),
        (
          "diagnostics",
          switch (result) {
          | AsYouType.SyntaxError(text, otherText, _) =>
            let errors =
              ErrorParser.parseErrors(
                Utils.splitLines(Utils.stripAnsii(otherText)),
              );
            let errors =
              errors
              |> List.filter(((_loc, message)) =>
                   message
                   != [
                        "Error: Uninterpreted extension 'merlin.syntax-error'.",
                      ]
                 );
            let errors =
              ErrorParser.parseErrors(
                Utils.splitLines(Utils.stripAnsii(text)),
              )
              @ errors;
            l(errors |> Utils.filterMap(makeDiagnostic(documentText)));
          | Success(text, _) =>
            if (String.trim(text) == "") {
              l([]);
            } else {
              let errors =
                ErrorParser.parseErrors(
                  Utils.splitLines(Utils.stripAnsii(text)),
                );
              l(errors |> Utils.filterMap(makeDiagnostic(documentText)));
            }
          | TypeError(text, _) =>
            Log.log("type error here " ++ text);
            let errors =
              ErrorParser.parseErrors(
                Utils.splitLines(Utils.stripAnsii(text)),
              )
              |> List.filter(((_loc, message)) => {
                   !
                     Str.string_match(
                       Str.regexp(
                         {|.*Missing dependency [a-zA-Z]+ in search path|},
                       ),
                       String.concat(" ", message),
                       0,
                     )
                 });

            l(errors |> Utils.filterMap(makeDiagnostic(documentText)));
          },
        ),
      ]),
    )
  );
};

let checkDocumentTimers = state => {
  let now = Unix.gettimeofday();
  let removed =
    Hashtbl.fold(
      (uri, timer, removed) =>
        if (now > timer) {
          switch (
            Packages.getPackage(
              ~reportDiagnostics=NotificationHandlers.reportDiagnostics,
              uri,
              state,
            )
          ) {
          | Ok(package) => runDiagnostics(uri, state, ~package)
          | Error(_) => () /* ignore... TODO should I do something */
          };
          [uri, ...removed];
        } else {
          removed;
        },
      state.documentTimers,
      [],
    );
  List.iter(uri => Hashtbl.remove(state.documentTimers, uri), removed);
  state;
};
