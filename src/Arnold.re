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
  | Call(call)
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

  let rec commandToString = command =>
    switch (command) {
    | Call(NamedArgument(namedArgument)) => "named:" ++ namedArgument
    | Call(ProgressFunction(progressFunction)) =>
      "progress:" ++ Path.name(progressFunction)
    | Call(RecursiveFunction({functionName, functionArgs})) =>
      functionName ++ functionArgsToString(functionArgs)
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
      Call(RecursiveFunction({functionName, functionArgs}));
    | Texp_apply({exp_desc: Texp_ident(callee, _, _)}, args)
        when callee |> isProgressFunction =>
      Call(ProgressFunction(callee))
    | Texp_apply({exp_desc: Texp_ident(callee, _, _)}, args)
        when
          functionTable
          |> FunctionTable.hasNamedArgument(
               ~functionName=currentFunctionName,
               ~namedArgument=Path.name(callee),
             ) =>
      Call(NamedArgument(Path.name(callee)))

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
        );

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