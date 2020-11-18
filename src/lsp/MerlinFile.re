let debug = ref(false);

let parseMerlin = text => {
  let lines = Str.split(Str.regexp_string("\n"), text);
  List.fold_left(
    ((source, build, flags), line) =>
      if (Utils.startsWith(line, "FLG ")) {
        (source, build, [Utils.chopPrefix(line, "FLG "), ...flags]);
      } else if (Utils.startsWith(line, "S ")) {
        ([Utils.chopPrefix(line, "S "), ...source], build, flags);
      } else if (Utils.startsWith(line, "B ")) {
        (source, [Utils.chopPrefix(line, "B "), ...build], flags);
      } else {
        (source, build, flags);
      },
    ([], [], []),
    lines,
  );
};

let getFlags = base =>
  RResult.InfixResult.(
    Files.readFile(base ++ "/.merlin")
    |> RResult.orError("no .merlin file")
    |?>> parseMerlin
    |?>> (((_, _, flags)) => flags |> List.rev)
  );
