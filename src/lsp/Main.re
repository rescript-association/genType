open Infix;
open RResult;

module StringSet = Set.Make(String);
let capabilities =
  JsonShort.(
    o([
      ("textDocumentSync", i(1)),
      ("hoverProvider", t),
      (
        "completionProvider",
        o([
          ("resolveProvider", t),
          /* TODO list # as trigger character */
          ("triggerCharacters", l([s(".")])),
        ]),
      ),
      ("signatureHelpProvider", o([("triggerCharacters", l([s("(")]))])),
      ("definitionProvider", t),
      ("typeDefinitionProvider", t),
      ("referencesProvider", t),
      ("documentSymbolProvider", t),
      /*
       * Found how to do the showReferences thing
       * https://github.com/Microsoft/vscode/blob/c6b1114292288e76e2901e05e860faf3a08b4b5a/extensions/typescript-language-features/src/features/implementationsCodeLensProvider.ts
       * but it seems I need to instantiate the object from javascript
       */
      ("codeActionProvider", t),
      (
        "executeCommandProvider",
        o([
          (
            "commands",
            l([s("reason-language-server.add_to_interface_inner")]),
          ),
        ]),
      ),
      ("codeLensProvider", o([("resolveProvider", t)])),
      ("documentHighlightProvider", t),
      ("documentRangeFormattingProvider", t),
      ("documentFormattingProvider", t),
      ("renameProvider", t),
    ])
  );

let getInitialState = params => {
  let uri =
    Json.get("rootUri", params) |?> Json.string |! "Must have a rootUri";
  let%try rootPath = uri |> Utils.parseUri |> resultOfOption("No root uri");

  Files.mkdirp(rootPath /+ "node_modules" /+ ".lsp");
  Log.setLocation(rootPath /+ "node_modules" /+ ".lsp" /+ "debug.log");
  Log.log("Hello - from " ++ Sys.executable_name);
  Log.log("Previous log location: " ++ Log.initial_dest);

  Rpc.sendNotification(
    stdout,
    "client/registerCapability",
    JsonShort.(
      o([
        (
          "registrations",
          l([
            o([
              ("id", s("watching")),
              ("method", s("workspace/didChangeWatchedFiles")),
              (
                "registerOptions",
                o([
                  (
                    "watchers",
                    l([
                      o([("globPattern", s("**/bsconfig.json"))]),
                      o([("globPattern", s("**/.merlin"))]),
                    ]),
                  ),
                ]),
              ),
            ]),
          ]),
        ),
      ])
    ),
  );

  /* if client needs plain text in any place, we disable markdown everywhere */
  let clientNeedsPlainText =
    !
      Infix.(
        Json.getPath("capabilities.textDocument.hover.contentFormat", params)
        |?> Protocol.hasMarkdownCap
        |? true
        && Json.getPath(
             "capabilities.textDocument.completion.completionItem.documentationFormat",
             params,
           )
        |?> Protocol.hasMarkdownCap
        |? true
      );

  let state = {...TopTypes.empty(), rootPath, rootUri: uri};

  Ok({
    ...state,
    settings: {
      ...state.settings,
      clientNeedsPlainText,
    },
  });
};

let tick = state => {
  NotificationHandlers.checkPackageTimers(state);
  Diagnostics.checkDocumentTimers(state);
};

let orLog = (message, v) =>
  switch (v) {
  | None =>
    print_endline(message);
    None;
  | Some(x) => Some(x)
  };

let processFile = (~state, ~uri, ~quiet) => {
  switch (Packages.getPackage(~reportDiagnostics=(_, _) => (), uri, state)) {
  | Error(message) =>
    print_endline("  Unable to get package: " ++ uri);
    print_endline(message);
    None;
  | Ok(package) =>
    switch (State.getCompilationResult(uri, state, ~package)) {
    | Error(message) =>
      print_endline("  Invalid compilation result: " ++ message);
      Some((package, None));
    | Ok(Success(_message, contents)) =>
      if (!quiet) {
        print_endline("  Good: " ++ uri);
      };
      Some((package, Some(contents)));
    | Ok(TypeError(message, _) | SyntaxError(message, _, _)) =>
      print_endline("  Error compiling: " ++ uri);
      print_endline(message);
      Some((package, None));
    }
  };
};

let singleDefinition = (~quiet, rootPath, filePath, line, col) => {
  Log.log(
    "# Reason Langauge Server - checking individual files to ensure they load & process correctly",
  );
  let rootPath =
    rootPath == "." ? Unix.getcwd() : maybeConcat(Unix.getcwd(), rootPath);
  let filePath = maybeConcat(Unix.getcwd(), filePath);
  let state = {
    ...TopTypes.empty(),
    rootPath,
    rootUri: Utils.toUri(rootPath),
  };

  let uri = Utils.toUri(filePath);
  switch (processFile(~state, ~uri, ~quiet)) {
  | Some((package, Some({file, extra}))) =>
    let _ = {
      let%opt_consume (location, loc) =
        References.locForPos(~extra, (line, col - 1))
        |> orLog(
             Printf.sprintf(
               "Nothing definable found at %s:%d:%d",
               filePath,
               line,
               col,
             ),
           );
      let%opt_consume (fname, dlocation) =
        References.definitionForLoc(
          ~pathsForModule=package.pathsForModule,
          ~file,
          ~getUri=State.fileForUri(state, ~package),
          ~getModule=State.fileForModule(state, ~package),
          loc,
        )
        |> orLog(
             Printf.sprintf(
               "Unable to resolve a definition for %s:%d:%d",
               filePath,
               location.loc_start.pos_lnum,
               location.loc_start.pos_cnum - location.loc_start.pos_bol + 1,
             ),
           );
      let%opt_consume fname = Utils.parseUri(fname);
      Printf.printf(
        "Definition for %s:%d:%d found at %s:%d:%d\n",
        filePath,
        location.loc_start.pos_lnum,
        location.loc_start.pos_cnum - location.loc_start.pos_bol + 1,
        fname,
        dlocation.loc_start.pos_lnum,
        dlocation.loc_start.pos_cnum - dlocation.loc_start.pos_bol + 1,
      );
    };
    print_endline("  Good: " ++ uri);
  | _ => ()
  };
};

let check = (~definitions, ~quiet, rootPath, files) => {
  Log.log(
    "# Reason Langauge Server - checking individual files to ensure they load & process correctly",
  );
  let rootPath =
    rootPath == "." ? Unix.getcwd() : maybeConcat(Unix.getcwd(), rootPath);
  let state = {
    ...TopTypes.empty(),
    rootPath,
    rootUri: Utils.toUri(rootPath),
  };
  files
  |> List.iter(filePath => {
       let filePath = maybeConcat(Unix.getcwd(), filePath);
       let uri = Utils.toUri(filePath);
       switch (processFile(~state, ~uri, ~quiet)) {
       | Some((package, result)) =>
         if (!definitions) {
           Log.log(State.Show.state(state, package));
         } else {
           switch (result) {
           | None => ()
           | Some({file, extra}) =>
             let missing = ref([]);
             extra.locations
             |> List.iter(((location, loc)) => {
                  switch (loc) {
                  | SharedTypes.Typed(_, LocalReference(tag, Type))
                      when tag <= 15 =>
                    ()
                  | Typed(
                      _,
                      GlobalReference(_, _, Constructor("[]" | "::")),
                    ) =>
                    ()
                  | Typed(
                      _,
                      (LocalReference(_, _) | GlobalReference(_, _, _)) as t,
                    )
                      when !location.Location.loc_ghost =>
                    switch (
                      References.definitionForLoc(
                        ~pathsForModule=package.pathsForModule,
                        ~file,
                        ~getUri=State.fileForUri(state, ~package),
                        ~getModule=State.fileForModule(state, ~package),
                        loc,
                      )
                    ) {
                    | None =>
                      // missing := 1 + missing^;
                      missing :=
                        [
                          Printf.sprintf(
                            "   - \"%s:%d:%d\" : %s",
                            filePath,
                            location.loc_start.pos_lnum,
                            location.loc_start.pos_cnum
                            - location.loc_start.pos_bol
                            + 1,
                            SharedTypes.locKindToString(t),
                          ),
                          ...missing^,
                        ]
                    | Some(_defn) => ()
                    }
                  | _ => ()
                  }
                });
             if (missing^ != []) {
               print_endline(filePath);
               print_endline(
                 "  > " ++ string_of_int(List.length(missing^)) ++ " missing",
               );
               missing^ |> List.iter(text => print_endline(text));
             };
           };
         }
       | _ => ()
       };
     });
  Log.log("Ok");
};

let dump = files => {
  Shared.cacheTypeToString := true;
  let rootPath = Unix.getcwd();
  let emptyState = TopTypes.empty();
  let state = {
    ...emptyState,
    rootPath,
    rootUri: Utils.toUri(rootPath),
    settings: {
      ...emptyState.settings,
      autoRebuild: false,
    },
  };
  files
  |> List.iter(filePath => {
       let (filePath, selectPos) =
         switch (filePath |> String.split_on_char(':')) {
         | [filePath, line, char] => (
             filePath,
             Some((line |> int_of_string, char |> int_of_string)),
           )
         | _ => (filePath, None)
         };
       let filePath = maybeConcat(Unix.getcwd(), filePath);
       let uri = Utils.toUri(filePath);
       switch (State.getFullFromCmt(uri, state)) {
       | Error(message) => print_endline(message)
       | Ok((package, {file, extra})) =>
         DumpCommand.dumpLocations(
           state,
           ~package,
           ~file,
           ~extra,
           ~selectPos,
           uri,
         )
       };
     });
};

let complete = (~pathWithPos, ~currentFile) => {
  let rootPath = Unix.getcwd();
  let emptyState = TopTypes.empty();
  let state = {
    ...emptyState,
    rootPath,
    rootUri: Utils.toUri(rootPath),
    settings: {
      ...emptyState.settings,
      autoRebuild: false,
    },
  };
  switch (pathWithPos |> String.split_on_char(':')) {
  | [filePath, line, char] =>
    let pos = (line |> int_of_string, char |> int_of_string);
    let filePath = maybeConcat(Unix.getcwd(), filePath);
    let uri = Utils.toUri(filePath);
    switch (State.getFullFromCmt(uri, state)) {
    | Error(message) => print_endline(message)
    | Ok((package, full)) =>
      Hashtbl.replace(state.lastDefinitions, uri, full);
      DumpCommand.autocomplete(~currentFile, ~full, ~package, ~pos, ~state);
    };
  | _ => ()
  };
};

let parseArgs = args => {
  switch (args) {
  | [] => assert(false)
  | [_, ...args] =>
    let (opts, pos) =
      args
      |> List.rev
      |> List.fold_left(
           ((set, pos), arg) =>
             if (arg != "" && arg.[0] == '-') {
               (set |> StringSet.add(arg), pos);
             } else {
               (set, [arg, ...pos]);
             },
           (StringSet.empty, []),
         );
    (opts, pos);
  };
};

let hasOpt = (opts, name) => opts |> StringSet.mem(name);

let hasOpts = (opts, names) => names |> List.exists(opts |> hasOpt);

let hasVerbose = opts => hasOpts(opts, ["-v", "--verbose"]);

let help = {|
Commands for Rescript Language Server

dump: compute definition and hover for Foo.res at line 0 and column 4:
bin.exe dump src/Foo.res:0:4

complete: compute autocomplete for Foo.res at line 0 and column 4,
  where Foo.res is being edited and the editor content is in file current.res.
bin.exe complete src/Foo.res:0:4 current.res

The dump command can also omit `:line:column`, to show results for every position in the file. Several files can be specified on the command line.
|};

let showHelp = () => {
  print_endline(help);
};

let main = () => {
  switch (parseArgs(Sys.argv |> Array.to_list)) {
  | (opts, _) when hasOpts(opts, ["-h", "--help"]) => showHelp()
  | (opts, []) =>
    if (opts |> hasVerbose) {
      Log.spamError := true;
      References.debugReferences := true;
      MerlinFile.debug := true;
    };
    Log.log("Booting up");
    BasicServer.run(
      ~tick,
      ~messageHandlers=MessageHandlers.handlers,
      ~notificationHandlers=NotificationHandlers.notificationHandlers,
      ~capabilities=_params => capabilities,
      ~getInitialState,
    );
    Log.log("Finished");
    Log.out^ |?< close_out;
  | (opts, ["definition", rootPath, file, line, col]) =>
    let line = int_of_string(line);
    let col = int_of_string(col);
    let quiet = hasOpts(opts, ["-q", "--quiet"]);
    if (opts |> hasVerbose) {
      Log.spamError := true;
      References.debugReferences := true;
      MerlinFile.debug := true;
    };
    singleDefinition(~quiet, rootPath, file, line, col);
  | (opts, ["check", rootPath, ...files]) =>
    let definitions = hasOpts(opts, ["-d", "--definitions"]);
    let quiet = hasOpts(opts, ["-q", "--quiet"]);
    if (opts |> hasVerbose) {
      Log.spamError := true;
      // if (!definitions) {
      MerlinFile.debug := true;
      // }
    } else {
      Log.spamError := false;
    };
    check(~definitions, ~quiet, rootPath, files);
  | (_opts, ["dump", ...files]) => dump(files)
  | (_opts, ["complete", pathWithPos, currentFile]) =>
    complete(~pathWithPos, ~currentFile)
  | _ =>
    showHelp();
    exit(1);
  };
};
