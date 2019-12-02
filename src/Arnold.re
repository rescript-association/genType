open DeadCommon;

type progressFunction = Path.t;

type namedArgument = string;

module Kind = {
  type t = list(namedArgument);

  let empty = [];

  let hasLabel = (~label, t: t) => List.mem(label, t);

  let toString = kind =>
    kind == [] ? "" : "<" ++ (kind |> String.concat(", ")) ++ ">";

  let addLabel = (~label, kind) =>
    if (!(kind |> List.mem(label))) {
      [label, ...kind] |> List.sort(compare);
    } else {
      kind;
    };
};

type functionName = string;

module FunctionArgs = {
  type arg = {
    label: string,
    functionName,
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
    functionName,
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

type call =
  | ProgressFunction(progressFunction)
  | FunctionCall(FunctionCall.t);

module Command = {
  type t =
    | Call(call, Lexing.position)
    | Sequence(list(t))
    | Nondet(list(t))
    | UnorderedSequence(list(t));

  let rec toString = command =>
    switch (command) {
    | Call(ProgressFunction(progressFunction), _pos) =>
      "+" ++ Path.name(progressFunction)
    | Call(FunctionCall(functionCall), _pos) =>
      FunctionCall.toString(functionCall)
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

module FunctionTable = {
  type functionDefinition = {
    mutable body: Command.t,
    mutable kind: Kind.t,
  };

  type t = Hashtbl.t(functionName, functionDefinition);

  let create = (): t => Hashtbl.create(1);

  let dump = (tbl: t) => {
    GenTypeCommon.logItem("\n  Function Table:\n");
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
         GenTypeCommon.logItem(
           "  %s%s: %s\n",
           functionName,
           Kind.toString(kind),
           Command.toString(body),
         )
       );
  };

  let initialFunctionDefinition = () => {
    kind: Kind.empty,
    body: Sequence([]),
  };

  let getFunctionDefinition = (~functionName, tbl: t) =>
    try(Hashtbl.find(tbl, functionName)) {
    | Not_found => assert(false)
    };

  let isFunctionInTable = (~functionTable, path) =>
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
      functionDefinition.kind |> Kind.addLabel(~label);
  };

  let addBody = (~body, ~functionName, tbl: t) => {
    let functionDefinition = tbl |> getFunctionDefinition(~functionName);
    functionDefinition.body = body;
  };

  let functionKindHasLabel = (~functionName, ~label, tbl: t) => {
    switch (Hashtbl.find(tbl, functionName)) {
    | {kind} => kind |> Kind.hasLabel(~label)
    | exception Not_found => false
    };
  };
};

module CallStack = {
  type t = {
    tbl: Hashtbl.t(FunctionCall.t, (int, Lexing.position)),
    mutable size: int,
  };

  let create = () => {tbl: Hashtbl.create(1), size: 0};

  let hasFunctionCall = (~functionCallInstantiated, t: t) =>
    Hashtbl.mem(t.tbl, functionCallInstantiated);

  let addFunctionCall = (~functionCall, ~pos, t: t) => {
    t.size = t.size + 1;
    Hashtbl.replace(t.tbl, functionCall, (t.size, pos));
  };

  let removeFunctionCall = (~functionCall, t: t) => {
    t.size = t.size - 1;
    Hashtbl.remove(t.tbl, functionCall);
  };

  let dump = (t: t) => {
    GenTypeCommon.logItem("  CallStack:\n");
    let frames =
      Hashtbl.fold(
        (functionCall, (i, pos), frames) =>
          [(functionCall, i, pos), ...frames],
        t.tbl,
        [],
      )
      |> List.sort(((_, i1, _), (_, i2, _)) => i2 - i1);
    frames
    |> List.iter(((functionCall: FunctionCall.t, i, pos)) =>
         GenTypeCommon.logItem(
           "  %d at %s (%s)\n",
           i,
           FunctionCall.toString(functionCall),
           pos |> posToString(~printCol=true, ~shortFile=true),
         )
       );
  };
};

module Eval = {
  module FunctionCallSet = Set.Make(FunctionCall);

  type cache = Hashtbl.t(FunctionCall.t, list((FunctionCallSet.t, bool)));

  let createCache = (): cache => Hashtbl.create(1);

  let functionCallSetOfCallStack = callStack =>
    Hashtbl.fold(
      (frame, _i, set) => FunctionCallSet.add(frame, set),
      callStack.CallStack.tbl,
      FunctionCallSet.empty,
    );

  let lookupCache = (~callStack, ~functionCall, cache: cache) => {
    switch (Hashtbl.find(cache, functionCall)) {
    | [] => None
    | results =>
      let set = functionCallSetOfCallStack(callStack);
      switch (
        results
        |> List.find(((cachedSet, _res)) =>
             FunctionCallSet.subset(set, cachedSet)
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

  let updateCache = (~callStack, ~functionCall, ~res, cache: cache) => {
    let set = functionCallSetOfCallStack(callStack);
    let modified = ref(false);
    let results =
      switch (Hashtbl.find(cache, functionCall)) {
      | results =>
        results
        |> List.map(((cachedSet, res)) => {
             let newFrameSet =
               if (FunctionCallSet.subset(cachedSet, set)) {
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
      functionCall,
      modified^ ? results : [(set, res), ...results],
    );
  };

  let hasInfiniteLoop =
      (~callStack, ~functionCall, ~functionCallInstantiated, ~pos) =>
    if (callStack |> CallStack.hasFunctionCall(~functionCallInstantiated)) {
      let explainCall =
        functionCall == functionCallInstantiated
          ? "\"" ++ (functionCall |> FunctionCall.toString) ++ "\""
          : "\""
            ++ (functionCall |> FunctionCall.toString)
            ++ "\" which is \""
            ++ (functionCallInstantiated |> FunctionCall.toString)
            ++ "\"";
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
            command: Command.t,
          ) =>
    switch (command) {
    | Call(FunctionCall({functionName} as functionCall), pos) =>
      let functionCallInstantiated =
        functionCall |> FunctionCall.applySubstitution(~sub=functionArgs);
      if (hasInfiniteLoop(
            ~callStack,
            ~functionCall,
            ~functionCallInstantiated,
            ~pos,
          )) {
        false; // continue as if it terminated without progress
      } else {
        switch (cache |> lookupCache(~callStack, ~functionCall)) {
        | Some(res) =>
          if (verbose) {
            GenTypeCommon.logItem(
              "%s termination analysis: cache hit for %s\n",
              pos |> posToString(~printCol=true, ~shortFile=true),
              FunctionCall.toString(functionCall),
            );
          };
          res;
        | None =>
          let functionDefinition =
            functionTable
            |> FunctionTable.getFunctionDefinition(~functionName);
          callStack |> CallStack.addFunctionCall(~functionCall, ~pos);
          let res =
            functionDefinition.body
            |> run(
                 ~cache,
                 ~callStack,
                 ~functionArgs=functionCall.functionArgs,
                 ~functionTable,
               );
          // Invariant: run should restore the callStack
          cache |> updateCache(~callStack, ~functionCall, ~res);
          callStack |> CallStack.removeFunctionCall(~functionCall);
          res;
        };
      };
    | Call(ProgressFunction(progressFunction), _pos) => true
    | Sequence(commands) =>
      // if one command makes progress, then the sequence makes progress
      commands
      |> List.exists(c =>
           c |> run(~cache, ~callStack, ~functionArgs, ~functionTable) == true
         )
    | UnorderedSequence(commands) =>
      // the commands could be executed in any order: progess if any one does
      let results =
        commands
        |> List.map(c =>
             c |> run(~cache, ~callStack, ~functionArgs, ~functionTable)
           );
      results |> List.mem(true);
    | Nondet(commands) =>
      let results =
        commands
        |> List.map(c =>
             c |> run(~cache, ~callStack, ~functionArgs, ~functionTable)
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
          when callee |> FunctionTable.isFunctionInTable(~functionTable) =>
        let functionName = Path.name(callee);
        args
        |> List.iter(((argLabel: Asttypes.arg_label, argOpt)) =>
             switch (argLabel, argOpt) {
             | (
                 Labelled(label),
                 Some({
                   Typedtree.exp_desc:
                     Texp_ident(path, {loc: {loc_start}}, _),
                 }),
               )
                 when path |> FunctionTable.isFunctionInTable(~functionTable) =>
               functionTable
               |> FunctionTable.addLabelToKind(~functionName, ~label);
               if (verbose) {
                 GenTypeCommon.logItem(
                   "%s termination analysis: \"%s\" is parametric ~%s=%s\n",
                   loc_start |> posToString(~printCol=true, ~shortFile=true),
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
        if (path |> FunctionTable.isFunctionInTable(~functionTable)) {
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
             | (Labelled(label), Some({Typedtree.exp_desc: Texp_ident(_)}))
                 when
                   functionTable
                   |> FunctionTable.functionKindHasLabel(
                        ~functionName=Path.name(path),
                        ~label,
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
    | Texp_ident(_) => Command.nothing
    | Texp_apply({exp_desc: Texp_ident(callee, _, _)}, args)
        when callee |> FunctionTable.isFunctionInTable(~functionTable) =>
      let functionName = Path.name(callee);
      let functionDefinition =
        functionTable |> FunctionTable.getFunctionDefinition(~functionName);
      exception ArgError;
      let getFunctionArg = label => {
        let argOpt =
          args
          |> List.find_opt(arg =>
               switch (arg) {
               | (Asttypes.Labelled(s), Some(e)) => s == label
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
              label,
            );
            raise(ArgError);
          | Some((_, Some({exp_desc: Texp_ident(path, _, _)})))
              when path |> FunctionTable.isFunctionInTable(~functionTable) =>
            let functionName = Path.name(path);
            {FunctionArgs.label, functionName};
          | _ =>
            GenTypeCommon.logItem(
              "%s termination error: named argument \"%s\" must be passed a recursive function\n",
              posString,
              label,
            );
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
    | Texp_apply({exp_desc: Texp_ident(callee, _, _)}, args)
        when callee |> isProgressFunction =>
      Command.Call(ProgressFunction(callee), pos) |> evalArgs(~args, ~ctx)
    | Texp_apply({exp_desc: Texp_ident(callee, _, _)}, args)
        when
          functionTable
          |> FunctionTable.functionKindHasLabel(
               ~functionName=currentFunctionName,
               ~label=Path.name(callee),
             ) =>
      Command.Call(
        FunctionCall(Path.name(callee) |> FunctionCall.noArgs),
        pos,
      )
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
    | Texp_function({cases}) =>
      cases |> List.map(case(~ctx)) |> Command.nondet

    | Texp_match(e, cases, [], _) =>
      Command.seq(
        e |> expression(~ctx),
        cases |> List.map(case(~ctx)) |> Command.nondet,
      )
    | Texp_match(_, _, [_, ..._] as _casesExn, _) => assert(false)

    | Texp_field(e, _lid, _desc) => e |> expression(~ctx)

    | Texp_try(_) => assert(false)
    | Texp_tuple(_) => assert(false)
    | Texp_variant(_) => assert(false)
    | Texp_record(_) => assert(false)
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
        let functionArgs = FunctionArgs.empty;
        let functionCall = FunctionCall.noArgs(functionName);
        callStack |> CallStack.addFunctionCall(~functionCall, ~pos);
        let functionDefinition =
          functionTable |> FunctionTable.getFunctionDefinition(~functionName);
        assert(functionDefinition.kind == Kind.empty);

        functionDefinition.body
        |> Eval.run(~cache, ~callStack, ~functionArgs, ~functionTable);
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