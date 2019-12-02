open DeadCommon;

type progressFunction = Path.t;

type namedArgument = string;

type namedArguments = list(namedArgument);

type functionName = string;

module FunctionArgs = {
  type arg = {
    namedArgument: string,
    functionName,
  };

  type t = list(arg);

  let argToString = ({namedArgument, functionName}) =>
    namedArgument ++ ":" ++ functionName;

  let toString = functionArgs => {
    functionArgs == []
      ? ""
      : "<"
        ++ (functionArgs |> List.map(argToString) |> String.concat(","))
        ++ ">";
  };

  let find = (t: t, ~namedArgument) =>
    switch (t |> List.find_opt(arg => arg.namedArgument == namedArgument)) {
    | Some({functionName}) => Some(functionName)
    | None => None
    };

  let compareArg = (a1, a2) => {
    let n = compare(a1.namedArgument, a2.namedArgument);
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

module RecursiveFunction = {
  type t = {
    functionName,
    functionArgs: FunctionArgs.t,
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

type call =
  | NamedArgument(namedArgument)
  | ProgressFunction(progressFunction)
  | RecursiveFunction(RecursiveFunction.t);

module Command = {
  type t =
    | Call(call, Lexing.position)
    | Sequence(list(t))
    | Nondet(list(t))
    | UnorderedSequence(list(t));

  let rec toString = command =>
    switch (command) {
    | Call(NamedArgument(namedArgument), _pos) => namedArgument
    | Call(ProgressFunction(progressFunction), _pos) =>
      "+" ++ Path.name(progressFunction)
    | Call(RecursiveFunction(recursiveFunction), _pos) =>
      RecursiveFunction.toString(recursiveFunction)
    | Sequence(commands) =>
      commands == []
        ? "_" : commands |> List.map(toString) |> String.concat("; ")
    | UnorderedSequence(commands) =>
      "{" ++ (commands |> List.map(toString) |> String.concat(", ")) ++ "}"
    | Nondet(commands) =>
      "( "
      ++ (commands |> List.map(toString) |> String.concat(" | "))
      ++ " )"
    };

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

type functionDefinition = {
  namedArguments,
  body: Command.t,
};

module FunctionTable = {
  type t = Hashtbl.t(functionName, functionDefinition);
  let create = (): t => Hashtbl.create(1);

  let namedArgumentsToString = namedArguments =>
    namedArguments == []
      ? "" : "<" ++ (namedArguments |> String.concat(", ")) ++ ">";

  let dump = (tbl: t) => {
    GenTypeCommon.logItem("  Function Table:\n");
    let definitions =
      Hashtbl.fold(
        (functionName, {namedArguments, body}, definitions) =>
          [(functionName, namedArguments, body), ...definitions],
        tbl,
        [],
      )
      |> List.sort(((fn1, _, _), (fn2, _, _)) => String.compare(fn1, fn2));
    definitions
    |> List.iter(((functionName, namedArguments, body)) =>
         GenTypeCommon.logItem(
           "  %s%s: %s\n",
           functionName,
           namedArgumentsToString(namedArguments),
           Command.toString(body),
         )
       );
    GenTypeCommon.logItem("\n");
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

module CallStack = {
  type t = {
    tbl: Hashtbl.t(RecursiveFunction.t, (int, Lexing.position)),
    mutable size: int,
  };

  let create = () => {tbl: Hashtbl.create(1), size: 0};

  let hasRecursiveFunction = (~recursiveFunction, t: t) =>
    Hashtbl.mem(t.tbl, recursiveFunction);

  let addRecursiveFunction = (~pos, ~recursiveFunction, t: t) => {
    t.size = t.size + 1;
    Hashtbl.replace(t.tbl, recursiveFunction, (t.size, pos));
  };

  let removeRecursiveFunction = (~recursiveFunction, t: t) => {
    t.size = t.size - 1;
    Hashtbl.remove(t.tbl, recursiveFunction);
  };

  let dump = (t: t) => {
    GenTypeCommon.logItem("  CallStack:\n");
    let frames =
      Hashtbl.fold(
        (recursiveFunction, (i, pos), frames) =>
          [(recursiveFunction, i, pos), ...frames],
        t.tbl,
        [],
      )
      |> List.sort(((_, i1, _), (_, i2, _)) => i2 - i1);
    frames
    |> List.iter(((recursiveFunction: RecursiveFunction.t, i, pos)) =>
         GenTypeCommon.logItem(
           "  %d at %s (%s)\n",
           i,
           RecursiveFunction.toString(recursiveFunction),
           pos |> posToString(~printCol=true, ~shortFile=true),
         )
       );
  };
};

module Eval = {
  module RecursiveFunctionSet = Set.Make(RecursiveFunction);

  type cache =
    Hashtbl.t(RecursiveFunction.t, list((RecursiveFunctionSet.t, bool)));

  let createCache = (): cache => Hashtbl.create(1);

  let recursiveFunctionSetOfCallStack = callStack =>
    Hashtbl.fold(
      (frame, _i, set) => RecursiveFunctionSet.add(frame, set),
      callStack.CallStack.tbl,
      RecursiveFunctionSet.empty,
    );

  let lookupCache = (~callStack, ~recursiveFunction, cache: cache) => {
    switch (Hashtbl.find(cache, recursiveFunction)) {
    | [] => None
    | results =>
      let set = recursiveFunctionSetOfCallStack(callStack);
      switch (
        results
        |> List.find(((cachedSet, _res)) =>
             RecursiveFunctionSet.subset(set, cachedSet)
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
    let set = recursiveFunctionSetOfCallStack(callStack);
    let modified = ref(false);
    let results =
      switch (Hashtbl.find(cache, recursiveFunction)) {
      | results =>
        results
        |> List.map(((cachedSet, res)) => {
             let newFrameSet =
               if (RecursiveFunctionSet.subset(cachedSet, set)) {
                 // termination on a bigger stack is a stronger result to cache
                 modified := true;
                 set;
               } else {
                 cachedSet;
               };
             (newFrameSet, res);
           })
      | exception Not_found => []
      };
    Hashtbl.replace(
      cache,
      recursiveFunction,
      modified^ ? results : [(set, res), ...results],
    );
  };

  let hasInfiniteLoop = (~namedArgument, ~callStack, ~pos, ~recursiveFunction) =>
    if (callStack |> CallStack.hasRecursiveFunction(~recursiveFunction)) {
      let explainCall =
        switch (namedArgument) {
        | None =>
          "\"" ++ (recursiveFunction |> RecursiveFunction.toString) ++ "\""
        | Some(arg) =>
          "\""
          ++ arg
          ++ "\" which is \""
          ++ (recursiveFunction |> RecursiveFunction.toString)
          ++ "\""
        };
      GenTypeCommon.logItem(
        "%s termination error: possilbe infinite loop when calling %s\n",
        pos |> posToString(~printCol=true, ~shortFile=true),
        explainCall,
      );
      if (verbose) {
        CallStack.dump(callStack);
      };
      true;
    } else {
      false;
    };

  let rec run =
          (
            ~cache: cache,
            ~callStack,
            ~functionArgs,
            ~functionTable,
            ~command: Command.t,
          ) =>
    switch (command) {
    | Call(RecursiveFunction({functionName} as recursiveFunction), pos) =>
      if (hasInfiniteLoop(
            ~callStack,
            ~namedArgument=None,
            ~pos,
            ~recursiveFunction,
          )) {
        false; // continue as if it terminated without progress
      } else {
        switch (cache |> lookupCache(~callStack, ~recursiveFunction)) {
        | Some(res) =>
          if (verbose) {
            GenTypeCommon.logItem(
              "%s termination analysis: cache hit for %s\n",
              pos |> posToString(~printCol=true, ~shortFile=true),
              RecursiveFunction.toString(recursiveFunction),
            );
          };
          res;
        | None =>
          let {namedArguments, body} =
            functionTable
            |> FunctionTable.getFunctionDefinition(~functionName);
          callStack
          |> CallStack.addRecursiveFunction(~pos, ~recursiveFunction);
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
      }
    | Call(ProgressFunction(progressFunction), _pos) => true
    | Call(NamedArgument(namedArgument), pos) =>
      let functionName =
        switch (functionArgs |> FunctionArgs.find(~namedArgument)) {
        | Some(functionName) => functionName
        | None => assert(false)
        };
      let recursiveFunction = RecursiveFunction.noArgs(functionName);
      if (hasInfiniteLoop(
            ~namedArgument=Some(namedArgument),
            ~callStack,
            ~pos,
            ~recursiveFunction,
          )) {
        false; // continue as if it terminated without progress
      } else {
        run(
          ~cache,
          ~callStack,
          ~functionArgs,
          ~functionTable,
          ~command=Call(RecursiveFunction(recursiveFunction), pos),
        );
      };
    | Sequence(commands) =>
      // if one command makes progress, then the sequence makes progress
      commands
      |> List.exists(c =>
           run(~cache, ~callStack, ~functionArgs, ~functionTable, ~command=c)
           == true
         )
    | UnorderedSequence(commands) =>
      // the commands could be executed in any order: progess if any one does
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
      results |> List.mem(true);
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
               if (verbose) {
                 GenTypeCommon.logItem(
                   "%s termination analysis: \"%s\" is parametric ~%s=%s\n",
                   loc_start |> posToString(~printCol=true, ~shortFile=true),
                   functionName,
                   namedArgument,
                   Path.name(labelArg),
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
            "%s termination error: \"%s\" can only be called directly, or passed as labeled argument.\n",
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
  type ctx = {
    currentFunctionName: functionName,
    functionTable: FunctionTable.t,
    isProgressFunction: Path.t => bool,
  };

  let rec expression = (~ctx, expr: Typedtree.expression) => {
    let {currentFunctionName, functionTable, isProgressFunction} = ctx;
    let pos = expr.exp_loc.loc_start;
    switch (expr.exp_desc) {
    | Texp_function({cases}) =>
      cases |> List.map(case(~ctx)) |> Command.nondet
    | Texp_ident(_) => Command.nothing
    | Texp_apply({exp_desc: Texp_ident(callee, _, _)}, args)
        when callee |> FunctionTable.isRecursiveFunction(~functionTable) =>
      let functionName = Path.name(callee);
      let {namedArguments} =
        functionTable |> FunctionTable.getFunctionDefinition(~functionName);
      exception ArgError;
      let getFunctionArg = namedArgument => {
        let argOpt =
          args
          |> List.find_opt(arg =>
               switch (arg) {
               | (Asttypes.Labelled(s), Some(e)) => s == namedArgument
               | _ => false
               }
             );
        let posString = pos |> posToString(~printCol=true, ~shortFile=true);
        let functionArg =
          switch (argOpt) {
          | None =>
            GenTypeCommon.logItem(
              "%s termination error: call must have named argument \"%s\"\n",
              posString,
              namedArgument,
            );
            raise(ArgError);
          | Some((_, Some({exp_desc: Texp_ident(path, _, _)})))
              when path |> FunctionTable.isRecursiveFunction(~functionTable) =>
            let functionName = Path.name(path);
            FunctionArgs.{namedArgument, functionName};
          | _ =>
            GenTypeCommon.logItem(
              "%s termination error: named argument \"%s\" must be passed a recursive function\n",
              posString,
              namedArgument,
            );
            raise(ArgError);
          };
        functionArg;
      };
      let functionArgsOpt =
        try(Some(namedArguments |> List.map(getFunctionArg))) {
        | ArgError => None
        };
      switch (functionArgsOpt) {
      | None => Command.nothing
      | Some(functionArgs) =>
        Command.Call(RecursiveFunction({functionName, functionArgs}), pos)
        |> evalArgs(~args, ~ctx)
      };
    | Texp_apply({exp_desc: Texp_ident(callee, _, _)}, args)
        when callee |> isProgressFunction =>
      Command.Call(ProgressFunction(callee), pos) |> evalArgs(~args, ~ctx)
    | Texp_apply({exp_desc: Texp_ident(callee, _, _)}, args)
        when
          functionTable
          |> FunctionTable.hasNamedArgument(
               ~functionName=currentFunctionName,
               ~namedArgument=Path.name(callee),
             ) =>
      Command.Call(NamedArgument(Path.name(callee)), pos)
      |> evalArgs(~args, ~ctx)

    | Texp_apply(expr, args) =>
      expr |> expression(~ctx) |> evalArgs(~args, ~ctx)
    | Texp_let(recFlag, valueBindings, inExpr) =>
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
    | Texp_construct(_loc, _desc, expressions) =>
      expressions
      |> List.map(e => e |> expression(~ctx))
      |> Command.unorderedSequence

    | Texp_match(_) => assert(false)
    | Texp_try(_) => assert(false)
    | Texp_tuple(_) => assert(false)
    | Texp_variant(_) => assert(false)
    | Texp_record(_) => assert(false)
    | Texp_field(_) => assert(false)
    | Texp_setfield(_) => assert(false)
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
    | Texp_assert(_) => assert(false)
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
    Command.unorderedSequence(commands @ [command]);
  }
  and case = (~ctx, {c_guard, c_rhs}: Typedtree.case) =>
    switch (c_guard) {
    | None => c_rhs |> expression(~ctx)
    | Some(e) =>
      Command.seq(e |> expression(~ctx), c_rhs |> expression(~ctx))
    };
};

let progressFunctionsFromAttributes = attributes => {
  let lidToString = lid => lid |> Longident.flatten |> String.concat(".");
  switch (attributes |> Annotation.getAttributePayload((==)("progress"))) {
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
  };
};

let traverseAst = {
  let super = Tast_mapper.default;

  let value_bindings = (self: Tast_mapper.mapper, (recFlag, valueBindings)) => {
    let progressFunctions =
      switch (valueBindings) {
      | _ when recFlag == Asttypes.Nonrecursive => []
      | [(valueBinding: Typedtree.value_binding), ..._] =>
        progressFunctionsFromAttributes(valueBinding.vb_attributes)
      | [] => []
      };

    if (progressFunctions != []) {
      let functionTable = FunctionTable.create();
      let isProgressFunction = path =>
        List.mem(Path.name(path), progressFunctions);

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

      if (verbose) {
        FunctionTable.dump(functionTable);
      };

      let (firstFunctionName, {Typedtree.exp_loc: {loc_start: pos}}) =
        recursiveDefinitions |> List.hd;
      let res = {
        let cache = Eval.createCache();
        let callStack = CallStack.create();
        let functionName = firstFunctionName;
        let functionArgs = [];
        let recursiveFunction = RecursiveFunction.noArgs(functionName);
        callStack |> CallStack.addRecursiveFunction(~pos, ~recursiveFunction);
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
      ignore(res);
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