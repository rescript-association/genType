let readFile = (~channel, ~onLine) => {
  let rec loop = () => {
    let line = input_line(channel);
    onLine(line);
    loop();
  };
  try(loop()) {
  | End_of_file => ()
  };
};

module Month: {
  type t;
  let compare: (t, t) => int;
  let fromDate: (~quarterly: bool, string) => t;
  let toString: (~csv: bool, ~quarterly: bool, t) => string;
} = {
  type t = {
    month: string,
    year: string,
  };
  let monthToNumber =
    fun
    | "Jan"
    | "Q1" => 1
    | "Feb"
    | "Q2" => 2
    | "Mar"
    | "Q3" => 3
    | "Apr"
    | "Q4" => 4
    | "May" => 5
    | "Jun" => 6
    | "Jul" => 7
    | "Aug" => 8
    | "Sep" => 9
    | "Oct" => 10
    | "Nov" => 11
    | "Dec" => 12
    | _ => assert(false);
  let toString = (~csv, ~quarterly, {month, year}) =>
    if (csv) {
      Printf.sprintf("%s-%s", month, year);
    } else if (quarterly) {
      Printf.sprintf(
        "%s_Q%d",
        String.sub(year, 2, 2),
        month |> monthToNumber,
      );
    } else {
      Printf.sprintf(
        "%s_%02d",
        String.sub(year, 2, 2),
        month |> monthToNumber,
      );
    };
  let fromDate = (~quarterly, date) => {
    let year = String.sub(date, 12, 4);
    let month = String.sub(date, 8, 3);
    let month =
      quarterly
        ? switch ((monthToNumber(month) + 2) / 3) {
          | 1 => "Q1"
          | 2 => "Q2"
          | 3 => "Q3"
          | 4 => "Q4"
          | _ => assert(false)
          }
        : month;

    {month, year};
  };
  let compare = ({month: m1, year: y1}, {month: m2, year: y2}) =>
    compare((y1, m1 |> monthToNumber), (y2, m2 |> monthToNumber));
};

module DiffsPerMonth = {
  type t = Hashtbl.t(Month.t, int);

  let create = (): t => Hashtbl.create(1);

  let add = (~month, tbl) => {
    let num =
      try(Hashtbl.find(tbl, month)) {
      | Not_found =>
        Hashtbl.replace(tbl, month, 0);
        0;
      };
    Hashtbl.replace(tbl, month, num + 1);
  };

  let printUsers = (~csv, ~diffsPerMonth, ~users) => {
    let sortedMonths =
      Hashtbl.fold((month, _num, acc) => [month, ...acc], diffsPerMonth, [])
      |> List.sort(Month.compare);
    let sortedUsers =
      Hashtbl.fold((user, _, acc) => [user, ...acc], users, [])
      |> List.sort(String.compare);
    sortedUsers
    |> List.iter(user => {
         let dpm = Hashtbl.find(users, user);
         let nums =
           sortedMonths
           |> List.map(month =>
                try(Hashtbl.find(dpm, month)) {
                | Not_found => 0
                }
              )
           |> List.map(n => Printf.sprintf("%5d", n))
           |> String.concat(csv ? ", " : " ");
         if (csv) {
           Printf.printf("%s, %s\n", user, nums);
         } else {
           Printf.printf("%15s %s\n", user, nums);
         };
       });
  };

  let print = (~csv, ~diffsPerMonth, ~quarterly, ~users) => {
    let sortedDiffs =
      Hashtbl.fold(
        (month, num, acc) => [(month, num), ...acc],
        diffsPerMonth,
        [],
      )
      |> List.sort(((m1, _), (m2, _)) => Month.compare(m1, m2));
    let months =
      sortedDiffs
      |> List.map(((m, _n)) => m |> Month.toString(~csv, ~quarterly))
      |> String.concat(csv ? ", " : " ");
    let nums =
      sortedDiffs
      |> List.map(((_m, n)) => Printf.sprintf("%5d", n))
      |> String.concat(csv ? ", " : " ");

    Printf.printf(
      "%15s %s\n%15s %s\n",
      csv ? "," : "",
      months,
      "total" ++ (csv ? "," : ""),
      nums,
    );
    printUsers(~csv, ~diffsPerMonth, ~users);
  };
};

module Diffs = {
  let users = Hashtbl.create(1);
  let diffsPerMonth = DiffsPerMonth.create();
  let currentFound = ref(false);
  let currentDate = ref("");
  let currentUser = ref("");

  let addItem = (~quarterly) => {
    // Mon, 28 Oct 2019
    let month = currentDate^ |> Month.fromDate(~quarterly);
    diffsPerMonth |> DiffsPerMonth.add(~month);
  };

  let processCurrentItem = (~quarterly) => {
    let user = currentUser^;
    let date = currentDate^;
    if (currentFound^) {
      let month = date |> Month.fromDate(~quarterly);
      diffsPerMonth |> DiffsPerMonth.add(~month);
      let dpm =
        try(Hashtbl.find(users, user)) {
        | Not_found =>
          let dpm = DiffsPerMonth.create();
          Hashtbl.replace(users, user, dpm);
          dpm;
        };
      dpm |> DiffsPerMonth.add(~month);
    };
  };

  let setUser = (~quarterly, user) => {
    processCurrentItem(~quarterly);
    let email =
      switch (String.index(user, '<'), String.index(user, '>')) {
      | (lt, gt) => String.sub(user, lt + 1, gt - lt - 1)
      | exception Not_found => user
      };
    let at = String.index(email, '@');

    currentUser := String.sub(email, 0, at);
    currentDate := "";
    currentFound := false;
  };
  let setDate = date => {
    currentDate := date;
  };
  let setFound = () => {
    currentFound := true;
  };

  let print = (~csv) => DiffsPerMonth.print(~csv, ~diffsPerMonth, ~users);
};

let run = () => {
  let processLine = (~extension, ~quarterly, line) => {
    let len = String.length(line);
    let user = "user:        ";
    let lenLhs = String.length(user);
    let handleExtension = () => {
      switch (extension) {
      | None => Diffs.setFound()
      | Some(s) =>
        try(
          {
            Str.search_forward(Str.regexp(Str.quote(s ++ " ")), line, 0)
            |> ignore;
            Diffs.setFound();
          }
        ) {
        | Not_found => ()
        }
      };
    };
    if (len >= lenLhs) {
      let lhs = String.sub(line, 0, lenLhs);
      let rhs = String.sub(line, lenLhs, len - lenLhs);
      switch (lhs) {
      | "user:        " => rhs |> Diffs.setUser(~quarterly)
      | "date:        " => rhs |> Diffs.setDate
      | "changeset:   " => ()
      | "summary:     " => ()
      | _ => handleExtension()
      };
    } else {
      handleExtension();
    };
  };

  // Only set when hg log --stat is used.
  // Some(".js") only counts .js files.
  let extension = None;

  let csv = true;

  let quarterly = true;

  readFile(~channel=stdin, ~onLine=processLine(~extension, ~quarterly));
  Diffs.processCurrentItem(~quarterly);
  Diffs.print(~csv, ~quarterly);
  exit(-1) |> ignore;
};

run();