val ast_to_comment :
  permissive:bool ->
  sections_allowed:Ast.sections_allowed ->
  parent_of_sections:Paths.Identifier.label_parent ->
  Ast.docs ->
    ((Comment.docs, Error.t) Error.result) Error.with_warnings
