open Infix;

let dumpLocations = (state, ~package, ~file, ~extra, ~selectPos, uri) => {
  let locations =
    extra.SharedTypes.locations
    |> List.filter(((l, _)) => !l.Location.loc_ghost);
  let locations = {
    switch (selectPos) {
    | Some(pos) =>
      let pos = Utils.cmtLocFromVscode(pos);
      switch (References.locForPos(~extra={...extra, locations}, pos)) {
      | None => []
      | Some(l) => [l]
      };
    | None => locations
    };
  };
  open JsonShort;
  let dedupTable = Hashtbl.create(1);
  let dedupHover = (hover, i) => {
    let isCandidate = String.length(hover) > 10;
    if (isCandidate) {
      switch (Hashtbl.find_opt(dedupTable, hover)) {
      | Some(n) => s("#" ++ string_of_int(n))
      | None =>
        Hashtbl.replace(dedupTable, hover, i);
        s(hover);
      };
    } else {
      s(hover);
    };
  };
  let locationsInfo =
    locations
    |> Utils.filterMapIndex((i, (location: Location.t, loc)) => {
         let locIsModule =
           switch (loc) {
           | SharedTypes.LModule(_)
           | TopLevelModule(_) => true
           | TypeDefinition(_)
           | Typed(_)
           | Constant(_)
           | Explanation(_) => false
           };

         let hoverText =
           Hover.newHover(
             ~rootUri=state.TopTypes.rootUri,
             ~file,
             ~getModule=State.fileForModule(state, ~package),
             ~markdown=!state.settings.clientNeedsPlainText,
             ~showPath=state.settings.showModulePathOnHover,
             loc,
           )
           |? "";
         let hover =
           hoverText == "" ? [] : [("hover", dedupHover(hoverText, i))];

         let uriLocOpt =
           References.definitionForLoc(
             ~pathsForModule=package.pathsForModule,
             ~file,
             ~getUri=State.fileForUri(state, ~package),
             ~getModule=State.fileForModule(state, ~package),
             loc,
           );
         let (def, skipZero) =
           switch (uriLocOpt) {
           | None => ([], false)
           | Some((uri2, loc)) =>
             let uriIsCurrentFile = uri == uri2;
             let posIsZero = ({Lexing.pos_lnum, pos_bol, pos_cnum}) =>
               pos_lnum == 1 && pos_cnum - pos_bol == 0;
             // Skip if range is all zero, unless it's a module
             let skipZero =
               !locIsModule
               && loc.loc_start
               |> posIsZero
               && loc.loc_end
               |> posIsZero;
             let range = ("range", Protocol.rangeOfLoc(loc));
             (
               [
                 (
                   "definition",
                   o(
                     uriIsCurrentFile
                       ? [range] : [("uri", Json.String(uri2)), range],
                   ),
                 ),
               ],
               skipZero,
             );
           };
         let skip = skipZero || hover == [] && def == [];
         skip
           ? None
           : Some(
               o([("range", Protocol.rangeOfLoc(location))] @ hover @ def),
             );
       })
    |> l;
  Log.spamError := true;
  Log.log(Json.stringify(locationsInfo));
  Log.spamError := false;
};

let autocomplete = (~currentFile, ~full, ~package, ~pos, ~state) => {
  let parameters =
    switch (Files.readFile(currentFile)) {
    | None => None
    | Some(text) =>
      switch (PartialParser.positionToOffset(text, pos)) {
      | None => None
      | Some(offset) =>
        switch (PartialParser.findCompletable(text, offset)) {
        | Nothing => None
        | Labeled(_) =>
          /* Not supported yet */
          None
        | Lident(string) => Some((text, offset, string))
        }
      }
    };
  let items =
    switch (parameters) {
    | None => []
    | Some((text, offset, string)) =>
      /* Log.log("Completing for string " ++ string); */
      let parts = Str.split(Str.regexp_string("."), string);
      let parts =
        string.[String.length(string) - 1] == '.' ? parts @ [""] : parts;
      let rawOpens = PartialParser.findOpens(text, offset);

      let allModules =
        package.TopTypes.localModules @ package.dependencyModules;
      let items =
        NewCompletions.get(
          ~full,
          ~package,
          ~rawOpens,
          ~getModule=State.fileForModule(state, ~package),
          ~allModules,
          pos,
          parts,
        );
      /* TODO(#107): figure out why we're getting duplicates. */
      Utils.dedup(items);
    };
  open JsonShort;
  let completions =
    items == []
      ? null
      : items
        |> List.map(
             (
               (
                 uri,
                 {
                   SharedTypes.name: {
                     txt: name,
                     loc: {loc_start: {pos_lnum}},
                   },
                   docstring,
                   item,
                 },
               ),
             ) => {
             o([
               ("label", s(name)),
               ("kind", i(NewCompletions.kindToInt(item))),
               ("detail", NewCompletions.detail(name, item) |> s),
               (
                 "documentation",
                 s(
                   (docstring |? "No docs")
                   ++ "\n\n"
                   ++ uri
                   ++ ":"
                   ++ string_of_int(pos_lnum),
                 ),
               ),
             ])
           })
        |> l;

  Log.spamError := true;
  Log.log(Json.stringify(completions));
  Log.spamError := false;
};
