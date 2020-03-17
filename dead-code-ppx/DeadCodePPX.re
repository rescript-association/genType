// Remove code annotated with @dead

let hasDeadAnnotation = attributes => {
  attributes
  |> List.exists((({txt}, payload): Parsetree.attribute) => txt == "dead");
};

let rec filter_map = (l, ~f) =>
  switch (l) {
  | [] => []
  | [x, ...rest] =>
    switch (f(x)) {
    | None => filter_map(rest, ~f)
    | Some(y) => [y, ...filter_map(rest, ~f)]
    }
  };

let value_binding_list = (mapper, value_bindings) =>
  value_bindings
  |> filter_map(~f=(value_binding: Parsetree.value_binding) =>
       if (value_binding.pvb_attributes |> hasDeadAnnotation) {
         None;
       } else {
         Some(
           Ast_mapper.default_mapper.value_binding(mapper, value_binding),
         );
       }
     );

let structure = (mapper, structure) => {
  structure
  |> filter_map(~f=(structure_item: Parsetree.structure_item) =>
       switch (structure_item.pstr_desc) {
       | Pstr_value(rec_value, value_bindings) =>
         let value_bindings = value_binding_list(mapper, value_bindings);
         if (value_bindings == []) {
           None;
         } else {
           Some({
             ...structure_item,
             pstr_desc: Pstr_value(rec_value, value_bindings),
           });
         };
       | Pstr_primitive({pval_attributes}) =>
         if (pval_attributes |> hasDeadAnnotation) {
           None;
         } else {
           Some(
             Ast_mapper.default_mapper.structure_item(mapper, structure_item),
           );
         }
       | _ =>
         Some(
           Ast_mapper.default_mapper.structure_item(mapper, structure_item),
         )
       }
     );
};

let signature = (mapper, signature) => {
  signature
  |> List.filter((signature_item: Parsetree.signature_item) =>
       switch (signature_item.psig_desc) {
       | Psig_value({pval_attributes}) =>
         !(pval_attributes |> hasDeadAnnotation)
       | _ => true
       }
     )
  |> List.map(Ast_mapper.default_mapper.signature_item(mapper));
};

let () =
  Ast_mapper.register("DeadPPX", _argv =>
    {...Ast_mapper.default_mapper, signature, structure}
  );