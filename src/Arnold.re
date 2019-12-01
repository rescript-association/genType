open DeadCommon;

let traverseAst = {
  let super = Tast_mapper.default;

  let inRecursion = ref(false);

  let expr = (self: Tast_mapper.mapper, e: Typedtree.expression) =>
    switch (e.exp_desc) {
    | Texp_ident(path, _, _) =>
      if (currentModuleName^ == "TestCyberTruck") {
        GenTypeCommon.logItem(
          "XXX inRecursion:%b path:%s\n",
          inRecursion^,
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

  let value_bindings = (self: Tast_mapper.mapper, (recFlag, valueBindings)) => {
    valueBindings
    |> List.iter((valueBinding: Typedtree.value_binding) => {
         switch (valueBinding.vb_pat.pat_desc) {
         | Tpat_var(id, _) =>
           if (recFlag == Asttypes.Recursive
               && currentModuleName^ == "TestCyberTruck") {
             GenTypeCommon.logItem(
               "XXX recursive definition:%s\n",
               Ident.name(id),
             );
           }
         | _ => ()
         };
         let saved = inRecursion^;
         inRecursion := recFlag == Asttypes.Recursive;
         super.value_binding(self, valueBinding) |> ignore;
         inRecursion := saved;
       });
    (recFlag, valueBindings);
  };

  Tast_mapper.{...super, expr, value_bindings};
};

let processStructure = (structure: Typedtree.structure) =>
  structure |> traverseAst.structure(traverseAst) |> ignore;