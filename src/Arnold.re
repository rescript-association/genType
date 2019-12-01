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

  let initialFunctionDefinition = {namedArguments: [], body: Sequence([])};

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
            "%s termination error: recursive function %s can only be called directly\n",
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

let traverseAst = {
  let super = Tast_mapper.default;

  let value_bindings = (self: Tast_mapper.mapper, (recFlag, valueBindings)) => {
    if (recFlag == Asttypes.Recursive && currentModuleName^ == "TestCyberTruck") {
      let functionTable = FunctionTable.create();

      valueBindings
      |> List.iter((valueBinding: Typedtree.value_binding) => {
           switch (valueBinding.vb_pat.pat_desc) {
           | Tpat_var(id, _) =>
             functionTable
             |> FunctionTable.addFunction(~functionName=Ident.name(id))
           | _ => ()
           };
           super.value_binding(self, valueBinding) |> ignore;
         });

      valueBindings
      |> List.iter((valueBinding: Typedtree.value_binding) => {
           switch (valueBinding.vb_pat.pat_desc) {
           | Tpat_var(id, _) =>
             if (recFlag == Asttypes.Recursive
                 && currentModuleName^ == "TestCyberTruck") {
               valueBinding.vb_expr
               |> NamedArgumentWithRecursiveFunction.check(~functionTable);
             }
           | _ => ()
           }
         });

      valueBindings
      |> List.iter((valueBinding: Typedtree.value_binding) => {
           switch (valueBinding.vb_pat.pat_desc) {
           | Tpat_var(id, _) =>
             if (recFlag == Asttypes.Recursive
                 && currentModuleName^ == "TestCyberTruck") {
               valueBinding.vb_expr
               |> ExpressionWellFormed.check(~functionTable);
             }
           | _ => ()
           }
         });
    };

    valueBindings
    |> List.iter((valueBinding: Typedtree.value_binding) => {
         super.value_binding(self, valueBinding) |> ignore
       });

    (recFlag, valueBindings);
  };

  Tast_mapper.{...super, value_bindings};
};

let processStructure = (structure: Typedtree.structure) =>
  structure |> traverseAst.structure(traverseAst) |> ignore;