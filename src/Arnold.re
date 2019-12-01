open DeadCommon;

type progressFunction = Path.t;

type namedArgument = string;

type namedArguments = list(namedArgument);

type functionName = string;

type functionArg = {
  namedArgument,
  functionName,
};

type functionArgs = list(functionArg);

type recursiveFunction = {
  functionName,
  functionArgs,
};

type call =
  | NamedArgument(namedArgument)
  | ProgressFunction(progressFunction)
  | RecursiveFunction(recursiveFunction);

type command =
  | Call(call, Lexing.position)
  | Sequence(list(command))
  | Nondet(list(command));

type functionDefinition = {
  namedArguments,
  body: command,
};

module FunctionTable = {
  type t = Hashtbl.t(functionName, functionDefinition);
  let create = (): t => Hashtbl.create(1);

  let functionArgToString = ({namedArgument, functionName}) =>
    namedArgument ++ ":" ++ functionName;

  let functionArgsToString = functionArgs => {
    functionArgs == []
      ? ""
      : functionArgs |> List.map(functionArgToString) |> String.concat(",");
  };

  let recursiveFunctionToString = ({functionName, functionArgs}) =>
    functionName ++ functionArgsToString(functionArgs);

  let rec commandToString = command =>
    switch (command) {
    | Call(NamedArgument(namedArgument), _pos) => "named:" ++ namedArgument
    | Call(ProgressFunction(progressFunction), _pos) =>
      "progress:" ++ Path.name(progressFunction)
    | Call(RecursiveFunction(recursiveFunction), _pos) =>
      recursiveFunctionToString(recursiveFunction)
    | Sequence(commands) =>
      commands == []
        ? "-" : commands |> List.map(commandToString) |> String.concat("; ")
    | Nondet(commands) =>
      "( "
      ++ (commands |> List.map(commandToString) |> String.concat(" | "))
      ++ " )"
    };

  let dump = (tbl: t) => {
    GenTypeCommon.logItem("Function Table:\n");
    Hashtbl.iter(
      (functionName, {namedArguments, body}) =>
        GenTypeCommon.logItem(
          "  %s%s: %s\n",
          functionName,
          namedArguments == []
            ? "" : "<" ++ (namedArguments |> String.concat(", ")) ++ ">",
          commandToString(body),
        ),
      tbl,
    );
  };

  let initialFunctionDefinition = {namedArguments: [], body: Sequence([])};

  let getFunctionDefinition = (~functionName, tbl: t) =>
    try(Hashtbl.find(tbl, functionName)) {
    | Not_found => assert(false)
    };

  let isRecursiveFunction = (~functionTable, path) =>
    Hashtbl.mem(functionTable, Path.name(path));

  let addFunction = (~functionName, tbl: t) => {
    if (Hashtbl.mem(tbl, functionName)) {
      assert(false);
    };
    Hashtbl.replace(tbl, functionName, initialFunctionDefinition);
  };

  let addNamedArgument = (~functionName, ~namedArgument, tbl: t) => {
    switch (Hashtbl.find(tbl, functionName)) {
    | {namedArguments} as functionDefinition =>
      if (!(namedArguments |> List.mem(namedArgument))) {
        Hashtbl.replace(
          tbl,
          functionName,
          {
            ...functionDefinition,
            namedArguments:
              [namedArgument, ...namedArguments] |> List.sort(compare),
          },
        );
      }
    | exception Not_found => assert(false)
    };
  };

  let addBody = (~body, ~functionName, tbl: t) => {
    switch (Hashtbl.find(tbl, functionName)) {
    | functionDefinition =>
      Hashtbl.replace(tbl, functionName, {...functionDefinition, body})
    | exception Not_found => assert(false)
    };
  };

  let hasNamedArgument = (~functionName, ~namedArgument, tbl: t) => {
    switch (Hashtbl.find(tbl, functionName)) {
    | {namedArguments} => namedArguments |> List.mem(namedArgument)
    | exception Not_found => false
    };
  };
};

module Frame = {
  type t = recursiveFunction;

  let compareFunctionArg = (a1, a2) => {
    let n = compare(a1.namedArgument, a2.namedArgument);
    if (n != 0) {
      n;
    } else {
      compare(a1.functionName, a2.functionName);
    };
  };

  let rec compareFunctionArgs = (l1, l2) =>
    switch (l1, l2) {
    | ([], []) => 0
    | ([], [_, ..._]) => (-1)
    | ([_, ..._], []) => 1
    | ([x1, ...l1], [x2, ...l2]) =>
      let n = compareFunctionArg(x1, x2);
      if (n != 0) {
        n;
      } else {
        compareFunctionArgs(l1, l2);
      };
    };

  let compare = (x1: t, x2) => {
    let n = compare(x1.functionName, x2.functionName);
    if (n != 0) {
      n;
    } else {
      compareFunctionArgs(x1.functionArgs, x2.functionArgs);
    };
  };
};

module CallStack = {
  type t = {
    tbl: Hashtbl.t(Frame.t, int),
    mutable size: int,
  };

  let create = () => {tbl: Hashtbl.create(1), size: 0};

  let hasRecursiveFunction = (~recursiveFunction, t: t) =>
    Hashtbl.mem(t.tbl, recursiveFunction);

  let addRecursiveFunction = (~recursiveFunction, t: t) => {
    t.size = t.size + 1;
    Hashtbl.replace(t.tbl, recursiveFunction, t.size);
  };

  let removeRecursiveFunction = (~recursiveFunction, t: t) => {
    t.size = t.size - 1;
    Hashtbl.remove(t.tbl, recursiveFunction);
  };

  let dump = (t: t) => {
    GenTypeCommon.logItem("CallStack:\n");
    Hashtbl.iter(
      (frame: Frame.t, i) =>
        GenTypeCommon.logItem(
          "  %d: %s\n",
          i,
          FunctionTable.recursiveFunctionToString(frame),
        ),
      t.tbl,
    );
  };
};

module Eval = {
  module FrameSet = Set.Make(Frame);

  type cache = Hashtbl.t(Frame.t, list((FrameSet.t, bool)));

  let createCache = (): cache => Hashtbl.create(1);

  let frameSetOfCallStack = callStack =>
    Hashtbl.fold(
      (frame, _i, set) => FrameSet.add(frame, set),
      callStack.CallStack.tbl,
      FrameSet.empty,
    );

  let lookupCache = (~callStack, ~recursiveFunction, cache: cache) => {
    switch (Hashtbl.find(cache, recursiveFunction)) {
    | [] => None
    | results =>
      let frameSet = frameSetOfCallStack(callStack);
      switch (
        results
        |> List.find(((cachedFrameSet, _res)) =>
             FrameSet.subset(frameSet, cachedFrameSet)
           )
      ) {
      | (_, res) =>
        // if it terminates on a stack, it terminates also on a smaller one
        Some(res)
      | exception Not_found => None
      };
    | exception Not_found => None
    };
  };

  let updateCache = (~callStack, ~recursiveFunction, ~res, cache: cache) => {
    let frameSet = frameSetOfCallStack(callStack);
    let modified = ref(false);
    let results =
      switch (Hashtbl.find(cache, recursiveFunction)) {
      | results =>
        results
        |> List.map(((cachedFrameSet, res)) => {
             let newFrameSet =
               if (FrameSet.subset(cachedFrameSet, frameSet)) {
                 // termination on a bigger stack is a stronger result to cache
                 modified := true;
                 frameSet;
               } else {
                 cachedFrameSet;
               };
             (newFrameSet, res);
           })
      | exception Not_found => []
      };
    Hashtbl.replace(
      cache,
      recursiveFunction,
      modified^ ? results : [(frameSet, res), ...results],
    );
  };

  let rec run =
          (~cache: cache, ~callStack, ~functionArgs, ~functionTable, ~command) =>
    switch (command) {
    | Call(RecursiveFunction(recursiveFunction), pos) =>
      let {functionName} = recursiveFunction;
      if (callStack |> CallStack.hasRecursiveFunction(~recursiveFunction)) {
        GenTypeCommon.logItem(
          "%s termination analysis: possilbe infinite loop on %s\n",
          pos |> posToString(~printCol=true, ~shortFile=true),
          functionName,
        );
        CallStack.dump(callStack);
        // continue as if it terminated without progress
        false;
      } else {
        switch (cache |> lookupCache(~callStack, ~recursiveFunction)) {
        | Some(res) => res
        | None =>
          let {namedArguments, body} =
            functionTable
            |> FunctionTable.getFunctionDefinition(~functionName);
          callStack |> CallStack.addRecursiveFunction(~recursiveFunction);
          let res =
            run(
              ~cache,
              ~callStack,
              ~functionArgs=recursiveFunction.functionArgs,
              ~functionTable,
              ~command=body,
            );
          // Invariant: run should restore the callStack
          cache |> updateCache(~callStack, ~recursiveFunction, ~res);
          callStack |> CallStack.removeRecursiveFunction(~recursiveFunction);
          res;
        };
      };
    | Call(ProgressFunction(progressFunction), _pos) => true
    | Call(NamedArgument(namedArgument), pos) =>
      let functionArgOpt =
        functionArgs
        |> List.find_opt(({namedArgument: n}) => n == namedArgument);
      let functionName =
        switch (functionArgOpt) {
        | Some({functionName}) => functionName
        | None => assert(false)
        };
      let recursiveFunction = {functionName, functionArgs: []};
      run(
        ~cache,
        ~callStack,
        ~functionArgs,
        ~functionTable,
        ~command=Call(RecursiveFunction(recursiveFunction), pos),
      );
    | Sequence(commands) =>
      let results =
        commands
        |> List.map(c =>
             run(
               ~cache,
               ~callStack,
               ~functionArgs,
               ~functionTable,
               ~command=c,
             )
           );
      // if one makes progress, then the sequence makes progress
      List.mem(true, results);
    | Nondet(commands) =>
      let results =
        commands
        |> List.map(c =>
             run(
               ~cache,
               ~callStack,
               ~functionArgs,
               ~functionTable,
               ~command=c,
             )
           );
      // make progress only if all the commands do
      !List.mem(false, results);
    };
};

module NamedArgumentWithRecursiveFunction = {
  let traverseExpr = (~functionTable) => {
    let super = Tast_mapper.default;

    let expr = (self: Tast_mapper.mapper, e: Typedtree.expression) => {
      switch (e.exp_desc) {
      | Texp_apply({exp_desc: Texp_ident(callee, _, _)}, args)
          when callee |> FunctionTable.isRecursiveFunction(~functionTable) =>
        let functionName = Path.name(callee);
        args
        |> List.iter(((argLabel: Asttypes.arg_label, argOpt)) =>
             switch (argLabel, argOpt) {
             | (
                 Labelled(namedArgument),
                 Some({
                   Typedtree.exp_desc:
                     Texp_ident(labelArg, {loc: {loc_start}}, _),
                 }),
               )
                 when
                   labelArg
                   |> FunctionTable.isRecursiveFunction(~functionTable) =>
               functionTable
               |> FunctionTable.addNamedArgument(
                    ~functionName,
                    ~namedArgument,
                  );
               GenTypeCommon.logItem(
                 "%s termination: recursive function %s has named argument %s taking recursive function %s\n",
                 loc_start |> posToString(~printCol=true, ~shortFile=true),
                 functionName,
                 namedArgument,
                 Path.name(labelArg),
               );

             | _ => ()
             }
           );

      | _ => ()
      };
      super.expr(self, e);
    };

    Tast_mapper.{...super, expr};
  };

  let check = (~functionTable, expression: Typedtree.expression) => {
    let traverseExpr = traverseExpr(~functionTable);
    expression |> traverseExpr.expr(traverseExpr) |> ignore;
  };
};

module ExpressionWellFormed = {
  let traverseExpr = (~functionTable) => {
    let super = Tast_mapper.default;

    let expr = (self: Tast_mapper.mapper, e: Typedtree.expression) =>
      switch (e.exp_desc) {
      | Texp_ident(path, {loc: {loc_start}}, _) =>
        if (path |> FunctionTable.isRecursiveFunction(~functionTable)) {
          GenTypeCommon.logItem(
            "%s termination error: checked recursive function \"%s\" can only be called directly, or passed as labeled argument to another checked recursive function.\n",
            loc_start |> posToString(~printCol=true, ~shortFile=true),
            Path.name(path),
          );
        };
        e;
      | Texp_apply({exp_desc: Texp_ident(path, _, _)}, args) =>
        args
        |> List.iter(((argLabel: Asttypes.arg_label, argOpt)) =>
             switch (argLabel, argOpt) {
             | (
                 Labelled(namedArgument),
                 Some({Typedtree.exp_desc: Texp_ident(_)}),
               )
                 when
                   functionTable
                   |> FunctionTable.hasNamedArgument(
                        ~functionName=Path.name(path),
                        ~namedArgument,
                      ) =>
               ()
             | (_, Some(arg)) => self.expr(self, arg) |> ignore
             | _ => ()
             }
           );
        e;

      | _ => super.expr(self, e)
      };

    Tast_mapper.{...super, expr};
  };

  let check = (~functionTable, expression: Typedtree.expression) => {
    let traverseExpr = traverseExpr(~functionTable);
    expression |> traverseExpr.expr(traverseExpr) |> ignore;
  };
};

module Compile = {
  let nothing = Sequence([]);

  let nondet = commands =>
    switch (commands) {
    | [] => nothing
    | [command] => command
    | _ => Nondet(commands)
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

  let nothing = Sequence([]);

  let rec sequence = commands =>
    switch (commands) {
    | [] => nothing
    | [command] => command
    | [command, ...nextCommands] => seq(command, sequence(nextCommands))
    };

  type ctx = {
    currentFunctionName: functionName,
    functionTable: FunctionTable.t,
    isProgressFunction: Path.t => bool,
  };

  let rec expression = (~ctx, expr: Typedtree.expression) => {
    let {currentFunctionName, functionTable, isProgressFunction} = ctx;
    let pos = expr.exp_loc.loc_start;
    switch (expr.exp_desc) {
    | Texp_function({cases}) => cases |> List.map(case(~ctx)) |> nondet
    | Texp_ident(_) => nothing
    | Texp_apply({exp_desc: Texp_ident(callee, _, _)}, args)
        when
          callee
          |> FunctionTable.isRecursiveFunction(
               ~functionTable=ctx.functionTable,
             ) =>
      let functionName = Path.name(callee);
      let {namedArguments} =
        functionTable |> FunctionTable.getFunctionDefinition(~functionName);
      let functionArgs =
        switch (namedArguments) {
        | [] => []
        | _ => assert(false) // TODO
        };
      Call(RecursiveFunction({functionName, functionArgs}), pos);
    | Texp_apply({exp_desc: Texp_ident(callee, _, _)}, args)
        when callee |> isProgressFunction =>
      Call(ProgressFunction(callee), pos)
    | Texp_apply({exp_desc: Texp_ident(callee, _, _)}, args)
        when
          functionTable
          |> FunctionTable.hasNamedArgument(
               ~functionName=currentFunctionName,
               ~namedArgument=Path.name(callee),
             ) =>
      Call(NamedArgument(Path.name(callee)), pos)

    | Texp_apply(expr, args) => expr |> expression(~ctx)
    | Texp_let(recFlag, valueBindings, inExpr) =>
      let commands =
        (
          valueBindings
          |> List.map((vb: Typedtree.value_binding) =>
               vb.vb_expr |> expression(~ctx)
             )
        )
        @ [inExpr |> expression(~ctx)];
      sequence(commands);
    | Texp_sequence(e1, e2) =>
      seq(e1 |> expression(~ctx), e2 |> expression(~ctx))
    | Texp_ifthenelse(e1, e2, eOpt) =>
      let c1 = e1 |> expression(~ctx);
      let c2 = e2 |> expression(~ctx);
      let c3 =
        switch (eOpt) {
        | None => nothing
        | Some(e3) => e3 |> expression(~ctx)
        };
      seq(c1, nondet([c2, c3]));
    | _ => assert(false)
    };
  }
  and case = (~ctx, {c_guard, c_rhs}: Typedtree.case) =>
    switch (c_guard) {
    | None => c_rhs |> expression(~ctx)
    | Some(e) => seq(e |> expression(~ctx), c_rhs |> expression(~ctx))
    };
};

let traverseAst = {
  let super = Tast_mapper.default;

  let value_bindings = (self: Tast_mapper.mapper, (recFlag, valueBindings)) => {
    if (recFlag == Asttypes.Recursive && currentModuleName^ == "TestCyberTruck") {
      let functionTable = FunctionTable.create();
      let isProgressFunction = path => Path.name(path) == "progress";

      let recursiveDefinitions =
        List.fold_left(
          (defs, valueBinding: Typedtree.value_binding) =>
            switch (valueBinding.vb_pat.pat_desc) {
            | Tpat_var(id, _) => [
                (Ident.name(id), valueBinding.vb_expr),
                ...defs,
              ]
            | _ => defs
            },
          [],
          valueBindings,
        )
        |> List.rev;

      recursiveDefinitions
      |> List.iter(((functionName, _expr)) => {
           functionTable |> FunctionTable.addFunction(~functionName)
         });

      recursiveDefinitions
      |> List.iter(((_, expr)) => {
           expr |> NamedArgumentWithRecursiveFunction.check(~functionTable)
         });

      recursiveDefinitions
      |> List.iter(((_, expr)) => {
           expr |> ExpressionWellFormed.check(~functionTable)
         });

      recursiveDefinitions
      |> List.iter(((functionName, expr)) => {
           let body =
             expr
             |> Compile.expression(
                  ~ctx={
                    currentFunctionName: functionName,
                    functionTable,
                    isProgressFunction,
                  },
                );
           functionTable |> FunctionTable.addBody(~body, ~functionName);
         });

      FunctionTable.dump(functionTable);

      let firstFunctionName = recursiveDefinitions |> List.hd |> fst;
      let res = {
        let cache = Eval.createCache();
        let callStack = CallStack.create();
        let functionName = firstFunctionName;
        let functionArgs = [];
        let recursiveFunction = {functionName, functionArgs};
        callStack |> CallStack.addRecursiveFunction(~recursiveFunction);
        let {namedArguments, body} =
          functionTable |> FunctionTable.getFunctionDefinition(~functionName);
        assert(namedArguments == []);

        Eval.run(
          ~cache,
          ~callStack,
          ~functionArgs,
          ~functionTable,
          ~command=body,
        );
      };

      GenTypeCommon.logItem(
        "termination analysis: Eval from %s returned %b\n",
        firstFunctionName,
        res,
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

let processStructure = (structure: Typedtree.structure) =>
  structure |> traverseAst.structure(traverseAst) |> ignore;