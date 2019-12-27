// Concrete Programs
// x y l f                   Variables
// arg := e | ~l=e           Function Argument
// args := args, ..., argn   Function Arguments
// e := x | e(args)          Expressions
// k := <l1:k1, ..., ln:kn>  Kind
// P := f1 = e1 ... fn = en  Program
// E := x1:k1, ..., xn:kn    Environment
//
// The variables xi in E must be distinct from labels in kj and in args.
//   E |- e  Well Formedness Relation
//   For fi = ei in P, and fi:ki in E, check E,ki |- ei.
//
//   E |- x if x:k not in E  No Escape
//
//   E |- x   E |- e
//   ---------------
//      E |- ~x=e
//
//   E |- x   E |- args
//   ------------------
//      E |- e(args)
//
//            E(x) = <l1:k1, ... ln:kn>
//         E(l1) = E(x1) ... E(ln) = E(xn)
//             E |- arg1 ... E |- argk
//   -------------------------------------------
//   E |- x(~l1:x1, ... ~ln:xn, arg1, ..., argk)
//
// Abstract Programs
// arg := l:x                Function Argument
// args := arg1, ...., argn  Function Arguments
// C ::=                     Commmand
//       x<args>             Call
//       Some | None         Optional Value
//       switch x<args> {    Switch on Optionals
//         | Some -> C1
//         | None -> C2 }
//       C1 ; ... ; Cn       Sequential Composition
//       C1 | ... | Cn       Nondeterministic Choice
//       { C1, ..., Cn }     No Evaluation Order
// FT := f1<args1> = C1      Function Table
//      ...
//      fn<argsn> = Cn
// Stack := f1<args1> ... fn<argsn>
// progress := Progress | NoProgress
// values := {some, none} -> ?progress
// State := (progress, ?values)
//
// Eval.run: (FT, Stack, f<args>, C, State) ---> State

let logItem = GenTypeCommon.logItem;

let posToString = DeadCommon.posToString;

let verbose = DeadCommon.verbose;

module StringSet = Set.Make(String);

// Type Definitions

module FunctionName = {
  type t = string;
};

module FunctionArgs = {
  type arg = {
    label: string,
    functionName: FunctionName.t,
  };

  type t = list(arg);

  let empty = [];

  let argToString = ({label, functionName}) => label ++ ":" ++ functionName;

  let toString = functionArgs => {
    functionArgs == []
      ? ""
      : "<"
        ++ (functionArgs |> List.map(argToString) |> String.concat(","))
        ++ ">";
  };

  let find = (t: t, ~label) =>
    switch (t |> List.find_opt(arg => arg.label == label)) {
    | Some({functionName}) => Some(functionName)
    | None => None
    };

  let compareArg = (a1, a2) => {
    let n = compare(a1.label, a2.label);
    if (n != 0) {
      n;
    } else {
      compare(a1.functionName, a2.functionName);
    };
  };

  let rec compare = (l1, l2) =>
    switch (l1, l2) {
    | ([], []) => 0
    | ([], [_, ..._]) => (-1)
    | ([_, ..._], []) => 1
    | ([x1, ...l1], [x2, ...l2]) =>
      let n = compareArg(x1, x2);
      if (n != 0) {
        n;
      } else {
        compare(l1, l2);
      };
    };
};

module FunctionCall = {
  type t = {
    functionName: FunctionName.t,
    functionArgs: FunctionArgs.t,
  };

  let substituteName = (~sub, name) => {
    switch (sub |> FunctionArgs.find(~label=name)) {
    | Some(functionName) => functionName
    | None => name
    };
  };

  let applySubstitution = (~sub: FunctionArgs.t, t: t) =>
    if (sub == []) {
      t;
    } else {
      {
        functionName: t.functionName |> substituteName(~sub),
        functionArgs:
          t.functionArgs
          |> List.map((arg: FunctionArgs.arg) =>
               {
                 ...arg,
                 functionName: arg.functionName |> substituteName(~sub),
               }
             ),
      };
    };

  let noArgs = functionName => {functionName, functionArgs: []};

  let toString = ({functionName, functionArgs}) =>
    functionName ++ FunctionArgs.toString(functionArgs);

  let compare = (x1: t, x2) => {
    let n = compare(x1.functionName, x2.functionName);
    if (n != 0) {
      n;
    } else {
      FunctionArgs.compare(x1.functionArgs, x2.functionArgs);
    };
  };
};

module FunctionCallSet = Set.Make(FunctionCall);

module Stats = {
  let nCacheChecks = ref(0);
  let nCacheHits = ref(0);
  let nFiles = ref(0);
  let nFunctions = ref(0);
  let nHygieneErrors = ref(0);
  let nInfiniteLoops = ref(0);
  let nRecursiveBlocks = ref(0);

  let dump = () => {
    logItem("\nTermination Analysis Stats\n");
    logItem("Files:%d\n", nFiles^);
    logItem("Recursive Blocks:%d\n", nRecursiveBlocks^);
    logItem("Functions:%d\n", nFunctions^);
    logItem("Infinite Loops:%d\n", nInfiniteLoops^);
    logItem("Hygiene Errors:%d\n", nHygieneErrors^);
    logItem("Cache Hits:%d/%d\n", nCacheHits^, nCacheChecks^);
  };

  let newFile = () => incr(nFiles);

  let newRecursiveFunctions = (~numFunctions) => {
    incr(nRecursiveBlocks);
    nFunctions := nFunctions^ + numFunctions;
  };

  let logLoop = (~explainCall, ~pos) => {
    incr(nInfiniteLoops);
    logItem(
      "%s termination error: possible infinite loop when calling %s\n",
      pos |> posToString,
      explainCall,
    );
  };

  let logCache = (~functionCall, ~hit, ~pos) => {
    incr(nCacheChecks);
    if (hit) {
      incr(nCacheHits);
    };
    if (verbose) {
      logItem(
        "%s termination analysis: cache %s for \"%s\"\n",
        pos |> posToString,
        hit ? "hit" : "miss",
        FunctionCall.toString(functionCall),
      );
    };
  };

  let logResult = (~functionCall, ~pos, ~resString) =>
    if (verbose) {
      logItem(
        "%s termination analysis: \"%s\" returns %s\n",
        pos |> posToString,
        FunctionCall.toString(functionCall),
        resString,
      );
    };

  let logHygieneParametric = (~functionName, ~pos) => {
    incr(nHygieneErrors);
    logItem(
      "%s hygiene error: \"%s\" cannot be analyzed directly as it is parametric\n",
      pos |> posToString,
      functionName,
    );
  };

  let logHygieneOnlyCallDirectly = (~path, ~pos) => {
    incr(nHygieneErrors);
    logItem(
      "%s hygiene error: \"%s\" can only be called directly, or passed as labeled argument.\n",
      pos |> posToString,
      Path.name(path),
    );
  };

  let logHygieneMustHaveNamedArgument = (~label, ~pos) => {
    incr(nHygieneErrors);
    logItem(
      "%s hygiene error: call must have named argument \"%s\"\n",
      pos |> posToString,
      label,
    );
  };

  let logHygieneNamedArgValue = (~label, ~pos) => {
    incr(nHygieneErrors);
    logItem(
      "%s hygiene error: named argument \"%s\" must be passed a recursive function\n",
      pos |> posToString,
      label,
    );
  };

  let logHygieneNoNestedLetRec = (~pos) => {
    incr(nHygieneErrors);
    logItem(
      "%s hygiene error: nested multiple let rec not supported yet\n",
      pos |> posToString,
    );
  };
};

module Progress = {
  type t =
    | Progress
    | NoProgress;

  let toString = progress => progress == Progress ? "Progress" : "NoProgress";
};

module Trace = {
  type progressFunction = Path.t;

  type call =
    | FunctionCall(FunctionCall.t)
    | ProgressFunction(progressFunction);

  type retOption =
    | Rsome
    | Rnone;

  type t =
    | Tcall(call, Progress.t)
    | Tnondet(list(t))
    | Toption(retOption)
    | Tseq(list(t));

  let callToString = call =>
    switch (call) {
    | ProgressFunction(progressFunction) =>
      "+" ++ Path.name(progressFunction)
    | FunctionCall(functionCall) => FunctionCall.toString(functionCall)
    };

  let empty = Tseq([]);

  let nd = (t1: t, t2: t): t =>
    switch (t1, t2) {
    | (Tnondet(l1), Tnondet(l2)) => Tnondet(l1 @ l2)
    | (_, Tnondet(l2)) => Tnondet([t1, ...l2])
    | (Tnondet(l1), _) => Tnondet(l1 @ [t2])
    | _ => Tnondet([t1, t2])
    };

  let seq = (t1: t, t2: t): t =>
    switch (t1, t2) {
    | (Tseq(l1), Tseq(l2)) => Tseq(l1 @ l2)
    | (_, Tseq(l2)) => Tseq([t1, ...l2])
    | (Tseq(l1), _) => Tseq(l1 @ [t2])
    | _ => Tseq([t1, t2])
    };

  let some = Toption(Rsome);
  let none = Toption(Rnone);

  let nondet = ts => Tnondet(ts);

  let retOptionToString = r => r == Rsome ? "Some" : "None";

  let rec toString = trace =>
    switch (trace) {
    | Tcall(ProgressFunction(progressFunction), progress) =>
      Path.name(progressFunction) ++ ":" ++ Progress.toString(progress)
    | Tcall(FunctionCall(functionCall), progress) =>
      FunctionCall.toString(functionCall)
      ++ ":"
      ++ Progress.toString(progress)
    | Tnondet(traces) =>
      "(" ++ (traces |> List.map(toString) |> String.concat(" | ")) ++ ")"
    | Toption(retOption) => retOption |> retOptionToString
    | Tseq(traces) =>
      let tracesNotEmpty = traces |> List.filter((!=)(empty));
      switch (tracesNotEmpty) {
      | [] => "_"
      | [t] => t |> toString
      | [_, ..._] =>
        "("
        ++ (tracesNotEmpty |> List.map(toString) |> String.concat("; "))
        ++ ")"
      };
    };
};

module Values = {
  type t = {
    none: option(Progress.t),
    some: option(Progress.t),
  };

  let getNone = ({none}) => none;
  let getSome = ({some}) => some;

  let toString = values =>
    (
      switch (values.some) {
      | None => []
      | Some(p) => ["some: " ++ Progress.toString(p)]
      }
    )
    @ (
      switch (values.none) {
      | None => []
      | Some(p) => ["none: " ++ Progress.toString(p)]
      }
    );

  let none = (~progress) => {none: Some(progress), some: None};
  let some = (~progress) => {none: None, some: Some(progress)};

  let nd = (v1: t, v2: t): t => {
    let combine = (x, y) =>
      switch (x, y) {
      | (Some(progress1), Some(progress2)) =>
        Some(
          progress1 == Progress.Progress && progress2 == Progress
            ? Progress.Progress : NoProgress,
        )
      | (None, progressOpt)
      | (progressOpt, None) => progressOpt
      };
    let none = combine(v1.none, v2.none);
    let some = combine(v1.some, v2.some);
    {none, some};
  };
};

module State = {
  type t = {
    progress: Progress.t,
    trace: Trace.t,
    valuesOpt: option(Values.t),
  };

  let toString = ({progress, trace, valuesOpt}) => {
    let progressStr =
      switch (valuesOpt) {
      | None => progress |> Progress.toString
      | Some(values) =>
        "{" ++ (values |> Values.toString |> String.concat(", ")) ++ "}"
      };
    progressStr ++ " with trace " ++ Trace.toString(trace);
  };

  let init =
      (~progress=Progress.NoProgress, ~trace=Trace.empty, ~valuesOpt=None, ()) => {
    progress,
    trace,
    valuesOpt,
  };

  let seq = (s1, s2) => {
    let progress =
      s1.progress == Progress || s2.progress == Progress
        ? Progress.Progress : NoProgress;
    let trace = {
      Trace.seq(s1.trace, s2.trace);
    };
    let valuesOpt = s2.valuesOpt;
    {progress, trace, valuesOpt};
  };

  let sequence = states =>
    switch (states) {
    | [] => assert(false)
    | [s, ...nextStates] => List.fold_left(seq, s, nextStates)
    };

  let nd = (s1, s2) => {
    let progress =
      s1.progress == Progress && s2.progress == Progress
        ? Progress.Progress : NoProgress;
    let trace = {
      Trace.nd(s1.trace, s2.trace);
    };
    let valuesOpt =
      switch (s1.valuesOpt, s2.valuesOpt) {
      | (None, valuesOpt) => s1.progress == Progress ? valuesOpt : None
      | (valuesOpt, None) => s2.progress == Progress ? valuesOpt : None
      | (Some(values1), Some(values2)) =>
        Some(Values.nd(values1, values2))
      };
    {progress, trace, valuesOpt};
  };

  let nondet = states =>
    switch (states) {
    | [] => assert(false)
    | [s, ...nextStates] => List.fold_left(nd, s, nextStates)
    };

  let unorderedSequence = states => {...states |> sequence, valuesOpt: None};

  let none = (~progress) =>
    init(
      ~progress,
      ~trace=Trace.none,
      ~valuesOpt=Some(Values.none(~progress)),
      (),
    );

  let some = (~progress) =>
    init(
      ~progress,
      ~trace=Trace.some,
      ~valuesOpt=Some(Values.some(~progress)),
      (),
    );
};

module Command = {
  type progress = Progress.t;

  type call = Trace.call;

  type retOption = Trace.retOption;

  type t =
    | Call(call, Lexing.position)
    | ConstrOption(retOption)
    | Nondet(list(t))
    | Sequence(list(t))
    | SwitchOption({
        functionCall: FunctionCall.t,
        pos: Lexing.position,
        some: t,
        none: t,
      })
    | UnorderedSequence(list(t));

  let rec toString = command =>
    switch (command) {
    | Call(call, _pos) => call |> Trace.callToString
    | ConstrOption(r) => r |> Trace.retOptionToString
    | Nondet(commands) =>
      "( "
      ++ (commands |> List.map(toString) |> String.concat(" | "))
      ++ " )"
    | Sequence(commands) =>
      commands == []
        ? "_"
        : "( "
          ++ (commands |> List.map(toString) |> String.concat("; "))
          ++ " )"
    | SwitchOption({functionCall, some: cSome, none: cNone}) =>
      "switch "
      ++ FunctionCall.toString(functionCall)
      ++ " {some: "
      ++ toString(cSome)
      ++ ", none: "
      ++ toString(cNone)
      ++ "}"
    | UnorderedSequence(commands) =>
      "{" ++ (commands |> List.map(toString) |> String.concat(", ")) ++ "}"
    };

  let nothing = Sequence([]);

  let nondet = commands => {
    let rec loop = commands =>
      switch (commands) {
      | [] => nothing
      | [Sequence([]), Sequence([]), ...rest] =>
        loop([Sequence([]), ...rest])
      | [Nondet(commands), ...rest] => loop(commands @ rest)
      | [Sequence([]), Nondet(commands), ...rest] =>
        loop([Sequence([]), ...commands] @ rest)
      | [command] => command
      | _ => Nondet(commands)
      };
    let (nothings, others) =
      commands |> List.partition(c => c == Sequence([]));
    loop(nothings == [] ? others : [Sequence([]), ...others]);
  };

  let seq = (c1, c2) =>
    switch (c1, c2) {
    | (Sequence([]), _) => c2
    | (_, Sequence([])) => c1
    | (Sequence(s1), Sequence(s2)) => Sequence(s1 @ s2)
    | (Sequence(s1), _) => Sequence(s1 @ [c2])
    | (_, Sequence(s2)) => Sequence([c1, ...s2])
    | _ => Sequence([c1, c2])
    };

  let rec sequence = commands =>
    switch (commands) {
    | [] => nothing
    | [command] => command
    | [command, ...nextCommands] => seq(command, sequence(nextCommands))
    };

  let unorderedSequence = commands =>
    switch (commands |> List.filter(x => x != nothing)) {
    | []
    | [_] => sequence(commands)
    | [_, _, ..._] => UnorderedSequence(commands)
    };
};

module Kind = {
  type t = list(entry)
  and entry = {
    label: string,
    k: t,
  };

  let empty: t = [];

  let hasLabel = (~label, k: t) =>
    k |> List.exists(entry => entry.label == label);

  let rec entryToString = ({label, k}) => {
    k == [] ? label : label ++ ":" ++ (k |> toString);
  }

  and toString = (kind: t) =>
    kind == []
      ? ""
      : "<"
        ++ (kind |> List.map(entryToString) |> String.concat(", "))
        ++ ">";

  let addLabelWithEmptyKind = (~label, kind) =>
    if (!(kind |> hasLabel(~label))) {
      [{label, k: empty}, ...kind] |> List.sort(compare);
    } else {
      kind;
    };
};

module FunctionTable = {
  type functionDefinition = {
    mutable body: option(Command.t),
    mutable kind: Kind.t,
  };

  type t = Hashtbl.t(FunctionName.t, functionDefinition);

  let create = (): t => Hashtbl.create(1);

  let dump = (tbl: t) => {
    logItem("\nFunction Table:\n");
    let definitions =
      Hashtbl.fold(
        (functionName, {kind, body}, definitions) =>
          [(functionName, kind, body), ...definitions],
        tbl,
        [],
      )
      |> List.sort(((fn1, _, _), (fn2, _, _)) => String.compare(fn1, fn2));
    definitions
    |> List.iter(((functionName, kind, body)) =>
         logItem(
           "  %s%s: %s\n",
           functionName,
           Kind.toString(kind),
           switch (body) {
           | Some(command) => Command.toString(command)
           | None => "None"
           },
         )
       );
  };

  let initialFunctionDefinition = () => {kind: Kind.empty, body: None};

  let getFunctionDefinition = (~functionName, tbl: t) =>
    try(Hashtbl.find(tbl, functionName)) {
    | Not_found => assert(false)
    };

  let isInFunctionInTable = (~functionTable, path) =>
    Hashtbl.mem(functionTable, Path.name(path));

  let addFunction = (~functionName, tbl: t) => {
    if (Hashtbl.mem(tbl, functionName)) {
      assert(false);
    };
    Hashtbl.replace(tbl, functionName, initialFunctionDefinition());
  };

  let addLabelToKind = (~functionName, ~label, tbl: t) => {
    let functionDefinition = tbl |> getFunctionDefinition(~functionName);
    functionDefinition.kind =
      functionDefinition.kind |> Kind.addLabelWithEmptyKind(~label);
  };

  let addBody = (~body, ~functionName, tbl: t) => {
    let functionDefinition = tbl |> getFunctionDefinition(~functionName);
    functionDefinition.body = body;
  };

  let functionGetKindOfLabel = (~functionName, ~label, tbl: t) => {
    switch (Hashtbl.find(tbl, functionName)) {
    | {kind} => kind |> Kind.hasLabel(~label) ? Some(Kind.empty) : None
    | exception Not_found => None
    };
  };
};

module FindFunctionsCalled = {
  let traverseExpr = (~callees) => {
    let super = Tast_mapper.default;

    let expr = (self: Tast_mapper.mapper, e: Typedtree.expression) => {
      switch (e.exp_desc) {
      | Texp_apply({exp_desc: Texp_ident(callee, _, _)}, args) =>
        let functionName = Path.name(callee);
        callees := StringSet.add(functionName, callees^);
      | _ => ()
      };
      super.expr(self, e);
    };

    Tast_mapper.{...super, expr};
  };

  let findCallees = (expression: Typedtree.expression) => {
    let isFunction =
      switch (expression.exp_desc) {
      | Texp_function(_) => true
      | _ => false
      };
    let callees = ref(StringSet.empty);
    let traverseExpr = traverseExpr(~callees);
    if (isFunction) {
      expression |> traverseExpr.expr(traverseExpr) |> ignore;
    };
    callees^;
  };
};

module ExtendFunctionTable = {
  // Add functions passed a recursive function via a labeled argument,
  // and functions calling progress functions, to the function table.

  let extractLabelledArgument =
      (~kindOpt=None, argOpt: option(Typedtree.expression)) =>
    switch (argOpt) {
    | Some({exp_desc: Texp_ident(path, {loc: {loc_start: pos}}, _)}) =>
      Some((path, pos))
    | Some({
        exp_desc:
          Texp_let(
            Nonrecursive,
            [
              {
                vb_pat: {pat_desc: Tpat_var(_, _)},
                vb_expr: {
                  exp_desc: Texp_ident(path, {loc: {loc_start: pos}}, _),
                },
                vb_loc: {loc_ghost: true},
              },
            ],
            _,
          ),
      }) =>
      Some((path, pos))
    | Some({
        exp_desc:
          Texp_apply(
            {exp_desc: Texp_ident(path, {loc: {loc_start: pos}}, _)},
            args,
          ),
      })
        when kindOpt != None =>
      let checkArg = ((argLabel: Asttypes.arg_label, argOpt)) => {
        switch (argLabel, kindOpt) {
        | (Labelled(l) | Optional(l), Some(kind)) =>
          kind |> List.for_all(({Kind.label}) => label != l)
        | _ => true
        };
      };
      if (args |> List.for_all(checkArg)) {
        Some((path, pos));
      } else {
        None;
      };
    | _ => None
    };

  let traverseExpr = (~functionTable, ~progressFunctions, ~valueBindingsTable) => {
    let super = Tast_mapper.default;

    let expr = (self: Tast_mapper.mapper, e: Typedtree.expression) => {
      switch (e.exp_desc) {
      | Texp_ident(callee, _, _) =>
        let pos = e.exp_loc.loc_start;
        switch (Hashtbl.find_opt(valueBindingsTable, Path.name(callee))) {
        | None => ()
        | Some((id_pos, _, callees)) =>
          if (!
                StringSet.is_empty(
                  StringSet.inter(Lazy.force(callees), progressFunctions),
                )) {
            let functionName = Path.name(callee);
            if (!(callee |> FunctionTable.isInFunctionInTable(~functionTable))) {
              functionTable |> FunctionTable.addFunction(~functionName);
              if (verbose) {
                logItem(
                  "%s termination analysis: extend Function Table with \"%s\" (%s) as it calls a progress function\n",
                  pos |> posToString,
                  functionName,
                  id_pos |> posToString,
                );
              };
            };
          }
        };
      | Texp_apply({exp_desc: Texp_ident(callee, _, _)}, args)
          when callee |> FunctionTable.isInFunctionInTable(~functionTable) =>
        let functionName = Path.name(callee);
        args
        |> List.iter(((argLabel: Asttypes.arg_label, argOpt)) =>
             switch (argLabel, argOpt |> extractLabelledArgument) {
             | (Labelled(label), Some((path, pos)))
                 when
                   path |> FunctionTable.isInFunctionInTable(~functionTable) =>
               functionTable
               |> FunctionTable.addLabelToKind(~functionName, ~label);
               if (verbose) {
                 logItem(
                   "%s termination analysis: \"%s\" is parametric ~%s=%s\n",
                   pos |> posToString,
                   functionName,
                   label,
                   Path.name(path),
                 );
               };

             | _ => ()
             }
           );
      | _ => ()
      };
      super.expr(self, e);
    };

    Tast_mapper.{...super, expr};
  };

  let run =
      (
        ~functionTable,
        ~progressFunctions,
        ~valueBindingsTable,
        expression: Typedtree.expression,
      ) => {
    let traverseExpr =
      traverseExpr(~functionTable, ~progressFunctions, ~valueBindingsTable);
    expression |> traverseExpr.expr(traverseExpr) |> ignore;
  };
};

module CheckExpressionWellFormed = {
  let traverseExpr = (~functionTable, ~valueBindingsTable) => {
    let super = Tast_mapper.default;

    let checkIdent = (~path, ~pos) =>
      if (path |> FunctionTable.isInFunctionInTable(~functionTable)) {
        Stats.logHygieneOnlyCallDirectly(~path, ~pos);
      };

    let expr = (self: Tast_mapper.mapper, e: Typedtree.expression) =>
      switch (e.exp_desc) {
      | Texp_ident(path, {loc: {loc_start: pos}}, _) =>
        checkIdent(~path, ~pos);
        e;
      | Texp_apply({exp_desc: Texp_ident(functionPath, _, _)}, args) =>
        let functionName = Path.name(functionPath);
        args
        |> List.iter(((argLabel: Asttypes.arg_label, argOpt)) => {
             switch (argOpt |> ExtendFunctionTable.extractLabelledArgument) {
             | Some((path, pos)) =>
               switch (argLabel) {
               | Labelled(label) =>
                 if (functionTable
                     |> FunctionTable.functionGetKindOfLabel(
                          ~functionName,
                          ~label,
                        )
                     != None) {
                   ();
                 } else {
                   switch (Hashtbl.find_opt(valueBindingsTable, functionName)) {
                   | Some((_pos, body: Typedtree.expression, _))
                       when
                         path
                         |> FunctionTable.isInFunctionInTable(~functionTable) =>
                     let inTable =
                       functionPath
                       |> FunctionTable.isInFunctionInTable(~functionTable);
                     if (!inTable) {
                       functionTable
                       |> FunctionTable.addFunction(~functionName);
                     };
                     functionTable
                     |> FunctionTable.addLabelToKind(~functionName, ~label);
                     if (verbose) {
                       logItem(
                         "%s termination analysis: extend Function Table with \"%s\" as parametric ~%s=%s\n",
                         body.exp_loc.loc_start |> posToString,
                         functionName,
                         label,
                         Path.name(path),
                       );
                     };

                   | _ => checkIdent(~path, ~pos)
                   };
                 }

               | Optional(_)
               | Nolabel => checkIdent(~path, ~pos)
               }

             | _ => ()
             }
           });
        e;

      | _ => super.expr(self, e)
      };

    Tast_mapper.{...super, expr};
  };

  let run =
      (~functionTable, ~valueBindingsTable, expression: Typedtree.expression) => {
    let traverseExpr = traverseExpr(~functionTable, ~valueBindingsTable);
    expression |> traverseExpr.expr(traverseExpr) |> ignore;
  };
};

module Compile = {
  type ctx = {
    currentFunctionName: FunctionName.t,
    functionTable: FunctionTable.t,
    innerRecursiveFunctions: Hashtbl.t(FunctionName.t, FunctionName.t),
    isProgressFunction: Path.t => bool,
  };

  let rec expression = (~ctx, expr: Typedtree.expression) => {
    let {currentFunctionName, functionTable, isProgressFunction} = ctx;
    let pos = expr.exp_loc.loc_start;
    switch (expr.exp_desc) {
    | Texp_ident(_) => Command.nothing

    | Texp_apply(
        {exp_desc: Texp_ident(calleeToRename, l, vd)} as expr,
        argsToExtend,
      ) =>
      let (callee, args) =
        switch (
          Hashtbl.find_opt(
            ctx.innerRecursiveFunctions,
            Path.name(calleeToRename),
          )
        ) {
        | Some(innerFunctionName) =>
          let innerFunctionDefinition =
            functionTable
            |> FunctionTable.getFunctionDefinition(
                 ~functionName=innerFunctionName,
               );
          let argsFromKind =
            innerFunctionDefinition.kind
            |> List.map((entry: Kind.entry) =>
                 (
                   Asttypes.Labelled(entry.label),
                   Some({
                     ...expr,
                     exp_desc:
                       Texp_ident(
                         Path.Pident(Ident.create(entry.label)),
                         l,
                         vd,
                       ),
                   }),
                 )
               );
          (
            Path.Pident(Ident.create(innerFunctionName)),
            argsFromKind @ argsToExtend,
          );
        | None => (calleeToRename, argsToExtend)
        };
      if (callee |> FunctionTable.isInFunctionInTable(~functionTable)) {
        let functionName = Path.name(callee);
        let functionDefinition =
          functionTable |> FunctionTable.getFunctionDefinition(~functionName);
        exception ArgError;
        let getFunctionArg = ({Kind.label}) => {
          let argOpt =
            args
            |> List.find_opt(arg =>
                 switch (arg) {
                 | (Asttypes.Labelled(s), Some(e)) => s == label
                 | _ => false
                 }
               );
          let argOpt =
            switch (argOpt) {
            | Some((_, Some(e))) => Some(e)
            | _ => None
            };
          let functionArg =
            switch (
              argOpt
              |> ExtendFunctionTable.extractLabelledArgument(
                   ~kindOpt=Some(functionDefinition.kind),
                 )
            ) {
            | None =>
              Stats.logHygieneMustHaveNamedArgument(~label, ~pos);
              raise(ArgError);

            | Some((path, _pos))
                when path |> FunctionTable.isInFunctionInTable(~functionTable) =>
              let functionName = Path.name(path);
              {FunctionArgs.label, functionName};

            | Some((path, _pos))
                when
                  functionTable
                  |> FunctionTable.functionGetKindOfLabel(
                       ~functionName=currentFunctionName,
                       ~label=Path.name(path),
                     )
                  == Some([]) /* TODO: when kinds are inferred, support and check non-empty kinds */ =>
              let functionName = Path.name(path);
              {FunctionArgs.label, functionName};

            | _ =>
              Stats.logHygieneNamedArgValue(~label, ~pos);
              raise(ArgError);
            };
          functionArg;
        };
        let functionArgsOpt =
          try(Some(functionDefinition.kind |> List.map(getFunctionArg))) {
          | ArgError => None
          };
        switch (functionArgsOpt) {
        | None => Command.nothing
        | Some(functionArgs) =>
          Command.Call(FunctionCall({functionName, functionArgs}), pos)
          |> evalArgs(~args, ~ctx)
        };
      } else if (callee |> isProgressFunction) {
        Command.Call(ProgressFunction(callee), pos) |> evalArgs(~args, ~ctx);
      } else {
        switch (
          functionTable
          |> FunctionTable.functionGetKindOfLabel(
               ~functionName=currentFunctionName,
               ~label=Path.name(callee),
             )
        ) {
        | Some(kind) when kind == Kind.empty =>
          Command.Call(
            FunctionCall(Path.name(callee) |> FunctionCall.noArgs),
            pos,
          )
          |> evalArgs(~args, ~ctx)
        | Some(_kind) =>
          // TODO when kinds are extended in future: check that args matches with kind
          // and create a function call with the appropriate arguments
          assert(false)
        | None => expr |> expression(~ctx) |> evalArgs(~args, ~ctx)
        };
      };
    | Texp_apply(expr, args) =>
      expr |> expression(~ctx) |> evalArgs(~args, ~ctx)
    | Texp_let(
        Recursive,
        [
          {
            vb_pat: {
              pat_desc: Tpat_var(id, _),
              pat_loc: {loc_start: funPos},
            },
            vb_expr,
          },
        ],
        inExpr,
      ) =>
      let oldFunctionName = Ident.name(id);
      let newFunctionName = currentFunctionName ++ "$" ++ oldFunctionName;
      functionTable
      |> FunctionTable.addFunction(~functionName=newFunctionName);
      let newFunctionDefinition =
        functionTable
        |> FunctionTable.getFunctionDefinition(~functionName=newFunctionName);
      let currentFunctionDefinition =
        functionTable
        |> FunctionTable.getFunctionDefinition(
             ~functionName=currentFunctionName,
           );
      newFunctionDefinition.kind = currentFunctionDefinition.kind;
      let newCtx = {...ctx, currentFunctionName: newFunctionName};
      Hashtbl.replace(
        ctx.innerRecursiveFunctions,
        oldFunctionName,
        newFunctionName,
      );
      newFunctionDefinition.body = Some(vb_expr |> expression(~ctx=newCtx));
      if (verbose) {
        logItem(
          "%s termination analysis: adding recursive definition \"%s\"\n",
          funPos |> posToString,
          newFunctionName,
        );
      };
      inExpr |> expression(~ctx);

    | Texp_let(recFlag, valueBindings, inExpr) =>
      if (recFlag == Recursive) {
        Stats.logHygieneNoNestedLetRec(~pos);
      };
      let commands =
        (
          valueBindings
          |> List.map((vb: Typedtree.value_binding) =>
               vb.vb_expr |> expression(~ctx)
             )
        )
        @ [inExpr |> expression(~ctx)];
      Command.sequence(commands);
    | Texp_sequence(e1, e2) =>
      Command.seq(e1 |> expression(~ctx), e2 |> expression(~ctx))
    | Texp_ifthenelse(e1, e2, eOpt) =>
      let c1 = e1 |> expression(~ctx);
      let c2 = e2 |> expression(~ctx);
      let c3 = eOpt |> expressionOpt(~ctx);
      Command.seq(c1, Command.nondet([c2, c3]));
    | Texp_constant(_) => Command.nothing
    | Texp_construct(
        {loc: {loc_start: pos, loc_ghost}},
        {cstr_name},
        expressions,
      ) =>
      let c =
        expressions
        |> List.map(e => e |> expression(~ctx))
        |> Command.unorderedSequence;
      switch (cstr_name) {
      | "Some" when loc_ghost == false => Command.seq(c, ConstrOption(Rsome))
      | "None" when loc_ghost == false => Command.seq(c, ConstrOption(Rnone))
      | _ => c
      };
    | Texp_function({cases}) =>
      cases |> List.map(case(~ctx)) |> Command.nondet

    | Texp_match(e, cases, [], _) =>
      let cE = e |> expression(~ctx);
      let cCases = cases |> List.map(case(~ctx));
      switch (cE, cases) {
      | (
          Call(FunctionCall(functionCall), pos),
          [
            {
              c_lhs: {
                pat_desc:
                  Tpat_construct(
                    _,
                    {cstr_name: ("Some" | "None") as name1},
                    _,
                  ),
              },
            },
            {
              c_lhs: {
                pat_desc: Tpat_construct(_, {cstr_name: "Some" | "None"}, _),
              },
            },
          ],
        ) =>
        let casesArr = Array.of_list(cCases);
        let (some, none) =
          name1 == "Some"
            ? (casesArr[0], casesArr[1]) : (casesArr[1], casesArr[0]);
        SwitchOption({functionCall, pos, some, none});
      | _ => Command.seq(cE, cCases |> Command.nondet)
      };

    | Texp_match(_, _, [_, ..._] as _casesExn, _) => assert(false)

    | Texp_field(e, _lid, _desc) => e |> expression(~ctx)

    | Texp_record({fields, extended_expression}) =>
      [
        extended_expression,
        ...fields
           |> Array.to_list
           |> List.map(
                (
                  (
                    _desc,
                    recordLabelDefinition: Typedtree.record_label_definition,
                  ),
                ) =>
                switch (recordLabelDefinition) {
                | Kept(_typeExpr) => None
                | Overridden(_loc, e) => Some(e)
                }
              ),
      ]
      |> List.map(expressionOpt(~ctx))
      |> Command.unorderedSequence

    | Texp_setfield(e1, _loc, _desc, e2) =>
      [e1, e2] |> List.map(expression(~ctx)) |> Command.unorderedSequence

    | Texp_tuple(expressions) =>
      expressions |> List.map(expression(~ctx)) |> Command.unorderedSequence

    | Texp_assert(_) => Command.nothing

    | Texp_try(_) => assert(false)
    | Texp_variant(_) => assert(false)
    | Texp_array(_) => assert(false)
    | Texp_while(_) => assert(false)
    | Texp_for(_) => assert(false)
    | Texp_send(_) => assert(false)
    | Texp_new(_) => assert(false)
    | Texp_instvar(_) => assert(false)
    | Texp_setinstvar(_) => assert(false)
    | Texp_override(_) => assert(false)
    | Texp_letmodule(_) => assert(false)
    | Texp_letexception(_) => assert(false)
    | Texp_lazy(_) => assert(false)
    | Texp_object(_) => assert(false)
    | Texp_pack(_) => assert(false)
    | Texp_unreachable => assert(false)
    | Texp_extension_constructor(_) => assert(false)
    };
  }
  and expressionOpt = (~ctx, eOpt) =>
    switch (eOpt) {
    | None => Command.nothing
    | Some(e) => e |> expression(~ctx)
    }
  and evalArgs = (~args, ~ctx, command) => {
    // Don't assume any evaluation order on the arguments
    let commands =
      args |> List.map(((_, eOpt)) => eOpt |> expressionOpt(~ctx));
    Command.seq(Command.unorderedSequence(commands), command);
  }
  and case = (~ctx, {c_guard, c_rhs}: Typedtree.case) =>
    switch (c_guard) {
    | None => c_rhs |> expression(~ctx)
    | Some(e) =>
      Command.seq(e |> expression(~ctx), c_rhs |> expression(~ctx))
    };
};

module CallStack = {
  type frame = {
    frameNumber: int,
    pos: Lexing.position,
  };

  type t = {
    tbl: Hashtbl.t(FunctionCall.t, frame),
    mutable size: int,
  };

  let create = () => {tbl: Hashtbl.create(1), size: 0};

  let toSet = ({tbl}) => {
    Hashtbl.fold(
      (frame, _i, set) => FunctionCallSet.add(frame, set),
      tbl,
      FunctionCallSet.empty,
    );
  };

  let hasFunctionCall = (~functionCall, t: t) =>
    Hashtbl.mem(t.tbl, functionCall);

  let addFunctionCall = (~functionCall, ~pos, t: t) => {
    t.size = t.size + 1;
    Hashtbl.replace(t.tbl, functionCall, {frameNumber: t.size, pos});
  };

  let removeFunctionCall = (~functionCall, t: t) => {
    t.size = t.size - 1;
    Hashtbl.remove(t.tbl, functionCall);
  };

  let dump = (t: t) => {
    logItem("  CallStack:\n");
    let frames =
      Hashtbl.fold(
        (functionCall, {frameNumber, pos}, frames) =>
          [(functionCall, frameNumber, pos), ...frames],
        t.tbl,
        [],
      )
      |> List.sort(((_, i1, _), (_, i2, _)) => i2 - i1);
    frames
    |> List.iter(((functionCall: FunctionCall.t, i, pos)) =>
         logItem(
           "  %d at %s (%s)\n",
           i,
           FunctionCall.toString(functionCall),
           pos |> posToString,
         )
       );
  };
};

module Eval = {
  type progress = Progress.t;

  type cache =
    Hashtbl.t(FunctionCall.t, list((FunctionCallSet.t, State.t)));

  let createCache = (): cache => Hashtbl.create(1);

  let lookupCache = (~callStack, ~functionCall, cache: cache) => {
    switch (Hashtbl.find(cache, functionCall)) {
    | [] => None
    | results =>
      let set = callStack |> CallStack.toSet;
      switch (
        results
        |> List.find(((cachedSet, _res)) =>
             FunctionCallSet.subset(set, cachedSet)
           )
      ) {
      | (_, callRes) =>
        // if it terminates on a stack, it terminates also on a smaller one
        Some(callRes)
      | exception Not_found => None
      };
    | exception Not_found => None
    };
  };

  let updateCache = (~callStack, ~functionCall, ~state, cache: cache) => {
    let set = callStack |> CallStack.toSet;
    let modified = ref(false);
    let results =
      switch (Hashtbl.find(cache, functionCall)) {
      | results =>
        results
        |> List.map(((cachedSet, res)) => {
             let newCachedSet =
               if (FunctionCallSet.subset(cachedSet, set)) {
                 // termination on a bigger stack is a stronger result to cache
                 modified := true;
                 set;
               } else {
                 cachedSet;
               };
             (newCachedSet, res);
           })
      | exception Not_found => []
      };
    Hashtbl.replace(
      cache,
      functionCall,
      modified^ ? results : [(set, state), ...results],
    );
  };

  let hasInfiniteLoop =
      (~callStack, ~functionCallToInstantiate, ~functionCall, ~pos, ~state0) =>
    if (callStack |> CallStack.hasFunctionCall(~functionCall)) {
      if (state0.State.progress == NoProgress) {
        let explainCall =
          functionCallToInstantiate == functionCall
            ? "\""
              ++ (functionCallToInstantiate |> FunctionCall.toString)
              ++ "\""
            : "\""
              ++ (functionCallToInstantiate |> FunctionCall.toString)
              ++ "\" which is \""
              ++ (functionCall |> FunctionCall.toString)
              ++ "\"";
        Stats.logLoop(~explainCall, ~pos);
        CallStack.dump(callStack);
      };
      true;
    } else {
      false;
    };

  let rec runFunctionCall =
          (
            ~cache,
            ~callStack,
            ~functionArgs,
            ~functionTable,
            ~madeProgressOn,
            ~pos,
            ~state0,
            functionCallToInstantiate,
          )
          : State.t => {
    let functionCall =
      functionCallToInstantiate
      |> FunctionCall.applySubstitution(~sub=functionArgs);
    let functionName = functionCall.functionName;
    let call = Trace.FunctionCall(functionCall);
    let state =
      switch (cache |> lookupCache(~callStack, ~functionCall)) {
      | Some((state: State.t)) =>
        Stats.logCache(~functionCall, ~hit=true, ~pos);
        let state = {...state, trace: Trace.Tcall(call, state.progress)};
        state;
      | None =>
        if (FunctionCallSet.mem(functionCall, madeProgressOn)) {
          let state =
            State.init(
              ~progress=Progress,
              ~trace=Trace.Tcall(call, Progress),
              (),
            );
          state;
        } else if (hasInfiniteLoop(
                     ~callStack,
                     ~functionCallToInstantiate,
                     ~functionCall,
                     ~pos,
                     ~state0,
                   )) {
          let trace = Trace.Tcall(call, state0.progress);
          {...state0, trace};
        } else {
          Stats.logCache(~functionCall, ~hit=false, ~pos);
          let functionDefinition =
            functionTable
            |> FunctionTable.getFunctionDefinition(~functionName);
          callStack |> CallStack.addFunctionCall(~functionCall, ~pos);
          let body =
            switch (functionDefinition.body) {
            | Some(body) => body
            | None => assert(false)
            };
          let state =
            body
            |> run(
                 ~cache,
                 ~callStack,
                 ~functionArgs=functionCall.functionArgs,
                 ~functionTable,
                 ~madeProgressOn,
                 ~state0=State.init(),
               );
          let resString = state |> State.toString;
          Stats.logResult(~functionCall, ~resString, ~pos);
          cache |> updateCache(~callStack, ~functionCall, ~state);
          // Invariant: run should restore the callStack
          callStack |> CallStack.removeFunctionCall(~functionCall);
          let trace = Trace.Tcall(call, state.progress);
          {...state, trace};
        }
      };
    State.seq(state0, state);
  }
  and run =
      (
        ~cache: cache,
        ~callStack,
        ~functionArgs,
        ~functionTable,
        ~madeProgressOn,
        ~state0,
        command: Command.t,
      )
      : State.t =>
    switch (command) {
    | Call(FunctionCall(functionCall), pos) =>
      functionCall
      |> runFunctionCall(
           ~cache,
           ~callStack,
           ~functionArgs,
           ~functionTable,
           ~madeProgressOn,
           ~pos,
           ~state0,
         )
    | Call(ProgressFunction(progressFunction) as call, _pos) =>
      let state =
        State.init(~progress=Progress, ~trace=Tcall(call, Progress), ());
      State.seq(state0, state);

    | ConstrOption(r) =>
      let state =
        r == Rsome
          ? State.some(~progress=state0.progress)
          : State.none(~progress=state0.progress);
      State.seq(state0, state);

    | Sequence(commands) =>
      // if one command makes progress, then the sequence makes progress
      let rec findFirstProgress =
              (~callStack, ~commands, ~madeProgressOn, ~state) =>
        switch (commands) {
        | [] => state
        | [c, ...nextCommands] =>
          let state1 =
            c
            |> run(
                 ~cache,
                 ~callStack,
                 ~functionArgs,
                 ~functionTable,
                 ~madeProgressOn,
                 ~state0=state,
               );
          let (madeProgressOn, callStack) =
            switch (state1.progress) {
            | Progress =>
              // look for infinite loops in the rest of the sequence, remembering what has made progress
              (
                FunctionCallSet.union(
                  madeProgressOn,
                  callStack |> CallStack.toSet,
                ),
                CallStack.create(),
              )
            | NoProgress => (madeProgressOn, callStack)
            };
          findFirstProgress(
            ~callStack,
            ~commands=nextCommands,
            ~madeProgressOn,
            ~state=state1,
          );
        };
      findFirstProgress(
        ~callStack,
        ~commands,
        ~madeProgressOn,
        ~state=state0,
      );

    | UnorderedSequence(commands) =>
      let state00 = {...state0, trace: Trace.empty};
      // the commands could be executed in any order: progess if any one does
      let states =
        commands
        |> List.map(c =>
             c
             |> run(
                  ~cache,
                  ~callStack,
                  ~functionArgs,
                  ~functionTable,
                  ~madeProgressOn,
                  ~state0=state00,
                )
           );
      State.seq(state0, states |> State.unorderedSequence);

    | Nondet(commands) =>
      let state00 = {...state0, trace: Trace.empty};
      // the commands could be executed in any order: progess if any one does
      let states =
        commands
        |> List.map(c =>
             c
             |> run(
                  ~cache,
                  ~callStack,
                  ~functionArgs,
                  ~functionTable,
                  ~madeProgressOn,
                  ~state0=state00,
                )
           );
      State.seq(state0, states |> State.nondet);

    | SwitchOption({functionCall, pos, some, none}) =>
      let state =
        functionCall
        |> runFunctionCall(
             ~cache,
             ~callStack,
             ~functionArgs,
             ~functionTable,
             ~madeProgressOn,
             ~pos,
             ~state0,
           );
      switch (state.valuesOpt) {
      | None =>
        Command.nondet([some, none])
        |> run(
             ~cache,
             ~callStack,
             ~functionArgs,
             ~functionTable,
             ~madeProgressOn,
             ~state0=state,
           )
      | Some(values) =>
        let runOpt = (c, progressOpt) =>
          switch (progressOpt) {
          | None => State.init(~progress=Progress, ())
          | Some(progress) =>
            c
            |> run(
                 ~cache,
                 ~callStack,
                 ~functionArgs,
                 ~functionTable,
                 ~madeProgressOn,
                 ~state0=State.init(~progress, ()),
               )
          };
        let stateNone = values |> Values.getNone |> runOpt(none);
        let stateSome = values |> Values.getSome |> runOpt(some);
        State.seq(state, State.nondet([stateSome, stateNone]));
      };
    };

  let analyzeFunction = (~cache, ~functionTable, ~pos, functionName) => {
    if (verbose) {
      logItem("\nTermination analysis for \"%s\"\n", functionName);
    };
    let callStack = CallStack.create();
    let functionArgs = FunctionArgs.empty;
    let functionCall = FunctionCall.noArgs(functionName);
    callStack |> CallStack.addFunctionCall(~functionCall, ~pos);
    let functionDefinition =
      functionTable |> FunctionTable.getFunctionDefinition(~functionName);
    if (functionDefinition.kind != Kind.empty) {
      Stats.logHygieneParametric(~functionName, ~pos);
    } else {
      let body =
        switch (functionDefinition.body) {
        | Some(body) => body
        | None => assert(false)
        };
      let state =
        body
        |> run(
             ~cache,
             ~callStack,
             ~functionArgs,
             ~functionTable,
             ~madeProgressOn=FunctionCallSet.empty,
             ~state0=State.init(),
           );
      let resString = state |> State.toString;
      Stats.logResult(~functionCall, ~resString, ~pos);
      cache |> updateCache(~callStack, ~functionCall, ~state);
    };
  };
};

let progressFunctionsFromAttributes = attributes => {
  let lidToString = lid => lid |> Longident.flatten |> String.concat(".");
  let isProgress = (==)("progress");
  if (attributes |> Annotation.hasAttribute(isProgress)) {
    Some(
      switch (attributes |> Annotation.getAttributePayload(isProgress)) {
      | None => []
      | Some(IdentPayload(lid)) => [lidToString(lid)]
      | Some(TuplePayload(l)) =>
        l
        |> List.filter(
             fun
             | Annotation.IdentPayload(_) => true
             | _ => false,
           )
        |> List.map(
             fun
             | Annotation.IdentPayload(lid) => lidToString(lid)
             | _ => assert(false),
           )
      | _ => []
      },
    );
  } else {
    None;
  };
};

let traverseAst = (~valueBindingsTable) => {
  let super = Tast_mapper.default;

  let value_bindings = (self: Tast_mapper.mapper, (recFlag, valueBindings)) => {
    // Update the table of value bindings for variables
    valueBindings
    |> List.iter((vb: Typedtree.value_binding) =>
         switch (vb.vb_pat.pat_desc) {
         | Tpat_var(id, {loc: {loc_start: pos}}) =>
           let callees = lazy(FindFunctionsCalled.findCallees(vb.vb_expr));
           Hashtbl.replace(
             valueBindingsTable,
             Ident.name(id),
             (pos, vb.vb_expr, callees),
           );
         | _ => ()
         }
       );

    let (progressFunctions, functionsToAnalyze) =
      if (recFlag == Asttypes.Nonrecursive) {
        (StringSet.empty, []);
      } else {
        let (progressFunctions0, functionsToAnalyze0) =
          valueBindings
          |> List.fold_left(
               (
                 (progressFunctions, functionsToAnalyze),
                 valueBinding: Typedtree.value_binding,
               ) =>
                 switch (
                   progressFunctionsFromAttributes(valueBinding.vb_attributes)
                 ) {
                 | None => (progressFunctions, functionsToAnalyze)
                 | Some(newProgressFunctions) => (
                     StringSet.union(
                       StringSet.of_list(newProgressFunctions),
                       progressFunctions,
                     ),
                     switch (valueBinding.vb_pat.pat_desc) {
                     | Tpat_var(id, _) => [
                         (
                           Ident.name(id),
                           valueBinding.vb_expr.exp_loc.loc_start,
                         ),
                         ...functionsToAnalyze,
                       ]
                     | _ => functionsToAnalyze
                     },
                   )
                 },
               (StringSet.empty, []),
             );
        (progressFunctions0, functionsToAnalyze0 |> List.rev);
      };

    if (functionsToAnalyze != []) {
      let functionTable = FunctionTable.create();
      let isProgressFunction = path =>
        StringSet.mem(Path.name(path), progressFunctions);

      let recursiveFunctions =
        List.fold_left(
          (defs, valueBinding: Typedtree.value_binding) =>
            switch (valueBinding.vb_pat.pat_desc) {
            | Tpat_var(id, _) => [Ident.name(id), ...defs]
            | _ => defs
            },
          [],
          valueBindings,
        )
        |> List.rev;
      let recursiveDefinitions =
        recursiveFunctions
        |> List.map(functionName =>
             (
               functionName,
               {
                 let (_pos, e, _set) =
                   Hashtbl.find(valueBindingsTable, functionName);
                 e;
               },
             )
           );

      recursiveDefinitions
      |> List.iter(((functionName, _body)) => {
           functionTable |> FunctionTable.addFunction(~functionName)
         });

      recursiveDefinitions
      |> List.iter(((_, body)) => {
           body
           |> ExtendFunctionTable.run(
                ~functionTable,
                ~progressFunctions,
                ~valueBindingsTable,
              )
         });

      recursiveDefinitions
      |> List.iter(((_, body)) => {
           body
           |> CheckExpressionWellFormed.run(
                ~functionTable,
                ~valueBindingsTable,
              )
         });

      functionTable
      |> Hashtbl.iter(
           (
             functionName,
             functionDefinition: FunctionTable.functionDefinition,
           ) =>
           if (functionDefinition.body == None) {
             let (_pos, body, _) =
               Hashtbl.find(valueBindingsTable, functionName);
             functionTable
             |> FunctionTable.addBody(
                  ~body=
                    Some(
                      body
                      |> Compile.expression(
                           ~ctx={
                             currentFunctionName: functionName,
                             functionTable,
                             innerRecursiveFunctions: Hashtbl.create(1),
                             isProgressFunction,
                           },
                         ),
                    ),
                  ~functionName,
                );
           }
         );

      if (verbose) {
        FunctionTable.dump(functionTable);
      };

      let cache = Eval.createCache();
      functionsToAnalyze
      |> List.iter(((functionName, pos)) =>
           functionName |> Eval.analyzeFunction(~cache, ~functionTable, ~pos)
         );
      Stats.newRecursiveFunctions(
        ~numFunctions=Hashtbl.length(functionTable),
      );
    };

    valueBindings
    |> List.iter(valueBinding => {
         super.value_binding(self, valueBinding) |> ignore
       });

    (recFlag, valueBindings);
  };

  Tast_mapper.{...super, value_bindings};
};

let processStructure = (structure: Typedtree.structure) => {
  Stats.newFile();
  let valueBindingsTable = Hashtbl.create(1);
  let traverseAst = traverseAst(~valueBindingsTable);
  structure |> traverseAst.structure(traverseAst) |> ignore;
};

let reportResults = () =>
  if (verbose) {
    Stats.dump();
  };