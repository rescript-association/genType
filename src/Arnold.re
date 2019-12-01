open DeadCommon;

module NamedArgumentWithRecursiveFunction = {
  let traverseExpr = (~isRecursiveFunction) => {
    let super = Tast_mapper.default;

    let expr = (self: Tast_mapper.mapper, e: Typedtree.expression) => {
      switch (e.exp_desc) {
      | Texp_apply({exp_desc: Texp_ident(path, _, _)}, args)
          when isRecursiveFunction(path) =>
        args
        |> List.iter(((argLabel: Asttypes.arg_label, argOpt)) =>
             switch (argLabel, argOpt) {
             | (
                 Labelled(s),
                 Some({
                   Typedtree.exp_desc:
                     Texp_ident(path1, {loc: {loc_start}}, _),
                 }),
               )
                 when isRecursiveFunction(path1) =>
               GenTypeCommon.logItem(
                 "%s termination: recursive function %s has labeled argument %s taking recursive function %s\n",
                 loc_start |> posToString(~printCol=true, ~shortFile=true),
                 Path.name(path),
                 s,
                 Path.name(path1),
               )

             | _ => ()
             }
           )

      | _ => ()
      };
      super.expr(self, e);
    };

    Tast_mapper.{...super, expr};
  };

  let check = (expression: Typedtree.expression, ~isRecursiveFunction) => {
    let traverseExpr = traverseExpr(~isRecursiveFunction);
    expression |> traverseExpr.expr(traverseExpr) |> ignore;
  };
};

module ExpressionWellFormed = {
  let traverseExpr = (~isRecursiveFunction) => {
    let super = Tast_mapper.default;

    let expr = (self: Tast_mapper.mapper, e: Typedtree.expression) =>
      switch (e.exp_desc) {
      | Texp_ident(path, {loc: {loc_start}}, _) =>
        if (isRecursiveFunction(path)) {
          GenTypeCommon.logItem(
            "%s termination error: recursive function %s can only be called directly\n",
            loc_start |> posToString(~printCol=true, ~shortFile=true),
            Path.name(path),
          );
        };
        e;
      | Texp_apply({exp_desc: Texp_ident(path, _, _)}, args) =>
        args
        |> List.iter(((_, argOpt)) =>
             switch (argOpt) {
             | Some(arg) => self.expr(self, arg) |> ignore
             | None => ()
             }
           );
        e;

      | _ => super.expr(self, e)
      };

    Tast_mapper.{...super, expr};
  };

  let check = (expression: Typedtree.expression, ~isRecursiveFunction) => {
    let traverseExpr = traverseExpr(~isRecursiveFunction);
    expression |> traverseExpr.expr(traverseExpr) |> ignore;
  };
};

let traverseAst = {
  let super = Tast_mapper.default;

  let currentRecursiveFunctions = ref(Hashtbl.create(1));

  let value_bindings = (self: Tast_mapper.mapper, (recFlag, valueBindings)) => {
    let saved = currentRecursiveFunctions^;
    let newFunctionTable = Hashtbl.create(1);
    currentRecursiveFunctions := newFunctionTable;
    if (recFlag == Asttypes.Recursive && currentModuleName^ == "TestCyberTruck") {
      valueBindings
      |> List.iter((valueBinding: Typedtree.value_binding) => {
           switch (valueBinding.vb_pat.pat_desc) {
           | Tpat_var(id, _) =>
             Hashtbl.add(currentRecursiveFunctions^, Ident.name(id), ())
           | _ => ()
           };
           super.value_binding(self, valueBinding) |> ignore;
         });
    };

    let isRecursiveFunction = path =>
      Hashtbl.mem(newFunctionTable, Path.name(path));

    valueBindings
    |> List.iter((valueBinding: Typedtree.value_binding) => {
         switch (valueBinding.vb_pat.pat_desc) {
         | Tpat_var(id, _) =>
           if (recFlag == Asttypes.Recursive
               && currentModuleName^ == "TestCyberTruck") {
             valueBinding.vb_expr
             |> ExpressionWellFormed.check(~isRecursiveFunction);
             valueBinding.vb_expr
             |> NamedArgumentWithRecursiveFunction.check(~isRecursiveFunction);
           }
         | _ => ()
         };
         super.value_binding(self, valueBinding) |> ignore;
       });
    currentRecursiveFunctions := saved;
    (recFlag, valueBindings);
  };

  Tast_mapper.{...super, value_bindings};
};

let processStructure = (structure: Typedtree.structure) =>
  structure |> traverseAst.structure(traverseAst) |> ignore;

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

type functionTable = Hashtbl.t(functionName, functionDefinition);

let createFunctionTable = (): functionTable => Hashtbl.create(1);