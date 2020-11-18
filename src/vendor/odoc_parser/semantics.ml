module Location = Location_
module Error = Error
module Comment = Comment

type 'a with_location = 'a Location.with_location



type status = {
  permissive : bool;
  mutable warnings : Error.t list;
  sections_allowed : Ast.sections_allowed;
  parent_of_sections : Paths.Identifier.label_parent;
}

let warning status message =
  if status.permissive then
    status.warnings <- message::status.warnings
  else
    Error.raise_exception message



(* TODO This and Token.describe probably belong in Parse_error. *)
let describe_element = function
  | `Reference (`Simple, _, _) ->
    Token.describe (`Simple_reference "")
  | `Reference (`With_text, _, _) ->
    Token.describe (`Begin_reference_with_replacement_text "")
  | `Link _ ->
    Token.describe (`Begin_link_with_replacement_text "")
  | `Heading (level, _, _) ->
    Token.describe (`Begin_section_heading (level, None))



let leaf_inline_element
    : status -> Comment.leaf_inline_element with_location ->
        Comment.leaf_inline_element with_location =
  fun status element ->

  begin match element.value with
  | `Code_span c as token ->
    begin match String.index c '\n' with
    | exception Not_found -> ()
    | _ ->
      Parse_error.not_allowed
        ~what:(Token.describe `Single_newline)
        ~in_what:(Token.describe token)
        element.location
      |> warning status
    end
  | _ -> ()
  end;

  element



let rec non_link_inline_element
    : status -> surrounding:_ -> Ast.inline_element with_location ->
        Comment.non_link_inline_element with_location =
    fun status ~surrounding element ->

  match element with
  | {value = #Comment.leaf_inline_element; _} as element ->
    let element = leaf_inline_element status element in
    (element :> Comment.non_link_inline_element with_location)

  | {value = `Styled (style, content); _} ->
    `Styled (style, non_link_inline_elements status ~surrounding content)
    |> Location.same element

  | {value = `Reference _; _}
  | {value = `Link _; _} as element ->
    Parse_error.not_allowed
      ~what:(describe_element element.value)
      ~in_what:(describe_element surrounding)
      element.location
    |> Error.raise_exception

and non_link_inline_elements status ~surrounding elements =
  List.map (non_link_inline_element status ~surrounding) elements



let rec inline_element
    : status -> Ast.inline_element with_location ->
        Comment.inline_element with_location =
    fun status element ->

  match element with
  | {value = #Comment.leaf_inline_element; _} as element ->
    (leaf_inline_element status element :> Comment.inline_element with_location)

  | {value = `Styled (style, content); location} ->
    `Styled (style, inline_elements status content)
    |> Location.at location

  | {value = `Reference (_, target, content) as value; location} ->
    `Reference
      (target, non_link_inline_elements status ~surrounding:value content)
    |> Location.at location

  | {value = `Link (target, content) as value; location} ->
    `Link (target, non_link_inline_elements status ~surrounding:value content)
    |> Location.at location

and inline_elements status elements =
  List.map (inline_element status) elements



let rec nestable_block_element
    : status -> Ast.nestable_block_element with_location ->
        Comment.nestable_block_element with_location =
    fun status element ->

  match element with
  | {value = `Paragraph content; location} ->
    Location.at location (`Paragraph (inline_elements status content))

  | {value = `Code_block _; _}
  | {value = `Doc _; _}
  | {value = `Example _; _}
  | {value = `Verbatim _; _}
  | {value = `Modules _; _} as element ->
    element

  | {value = `List (kind, items); location} ->
    `List (kind, List.map (nestable_block_elements status) items)
    |> Location.at location

and nestable_block_elements status elements =
  List.map (nestable_block_element status) elements



let tag : status -> Ast.tag -> Comment.tag = fun status tag ->
  match tag with
  | `Author _
  | `Since _
  | `Version _
  | `Canonical _
  | `Inline
  | `Open
  | `Closed as tag ->
    tag

  | `Deprecated content ->
    `Deprecated (nestable_block_elements status content)

  | `Param (name, content) ->
    `Param (name, nestable_block_elements status content)

  | `Raise (name, content) ->
    `Raise (name, nestable_block_elements status content)

  | `Return content ->
    `Return (nestable_block_elements status content)

  | `See (kind, target, content) ->
    `See (kind, target, nestable_block_elements status content)

  | `Before (version, content) ->
    `Before (version, nestable_block_elements status content)



(* When the user does not give a section heading a label (anchor), we generate
   one from the text in the heading. This is the common case. This involves
   simply scanning the AST for words, lowercasing them, and joining them with
   hyphens.

   This must be done in the parser (i.e. early, not at HTML/other output
   generation time), so that the cross-referencer can see these anchors. *)
let generate_heading_label : Comment.link_content -> string = fun content ->

  (* Code spans can contain spaces, so we need to replace them with hyphens. We
     also lowercase all the letters, for consistency with the rest of this
     procedure. *)
  let replace_spaces_with_hyphens_and_lowercase s =
    let result = Bytes.create (String.length s) in
    s |> String.iteri begin fun index c ->
      let c =
        match c with
        | ' ' | '\t' | '\r' | '\n' -> '-'
        | _ -> Char.lowercase_ascii c
      in
      Bytes.set result index c
    end;
    Bytes.unsafe_to_string result
  in

  (* Perhaps this should be done using a [Buffer.t]; we can switch to that as
     needed. *)
  let rec scan_inline_elements anchor = function
    | [] ->
      anchor
    | element::more ->
      let anchor =
        match element.Location.value with
        | `Space ->
          anchor ^ "-"
        | `Word w ->
          anchor ^ (String.lowercase_ascii w)
        | `Code_span c ->
          anchor ^ (replace_spaces_with_hyphens_and_lowercase c)
        | `Styled (_, content) ->
          scan_inline_elements anchor content
      in
      scan_inline_elements anchor more
  in
  scan_inline_elements "" content

let section_heading
    : status ->
      parsed_a_title:bool ->
      Location.span ->
      int ->
      string option ->
      (Ast.inline_element with_location) list ->
        bool * (Comment.block_element with_location) =
    fun status ~parsed_a_title location level label content ->

  let content =
    non_link_inline_elements
      status ~surrounding:(`Heading (level, label, content)) content
  in

  let label =
    match label with
    | Some label -> label
    | None -> generate_heading_label content
  in
  let label = Paths.Identifier.Label (status.parent_of_sections, label) in

  match status.sections_allowed, level with
  | `None, _ ->
    warning status (Parse_error.sections_not_allowed location);
    let content = (content :> (Comment.inline_element with_location) list) in
    let element =
      Location.at location
        (`Paragraph [Location.at location
          (`Styled (`Bold, content))])
    in
    parsed_a_title, element

  | `All, 1 ->
    if parsed_a_title then
      Error.raise_exception (Parse_error.only_one_title_allowed location);
    let element = `Heading (`Title, label, content) in
    let element = Location.at location element in
    true, element

  | _ ->
    let level =
      match level with
      | 2 -> `Section
      | 3 -> `Subsection
      | 4 -> `Subsubsection
      | _ ->
        Parse_error.bad_section_level (string_of_int level) location
        |> warning status;
        if level < 2 then
          `Section
        else
          `Subsubsection
    in
    let element = `Heading (level, label, content) in
    let element = Location.at location element in
    parsed_a_title, element



let top_level_block_elements
    : status -> (Ast.block_element with_location) list ->
        (Comment.block_element with_location) list =
    fun status ast_elements ->

  let rec traverse
      : parsed_a_title:bool ->
        (Comment.block_element with_location) list ->
        (Ast.block_element with_location) list ->
          (Comment.block_element with_location) list =
      fun ~parsed_a_title comment_elements_acc ast_elements ->

    match ast_elements with
    | [] ->
      List.rev comment_elements_acc

    | ast_element::ast_elements ->
      match ast_element with
      | {value = #Ast.nestable_block_element; _} as element ->
        let element = nestable_block_element status element in
        let element = (element :> Comment.block_element with_location) in
        traverse ~parsed_a_title (element::comment_elements_acc) ast_elements

      | {value = `Tag the_tag; _} ->
        let element = Location.same ast_element (`Tag (tag status the_tag)) in
        traverse ~parsed_a_title (element::comment_elements_acc) ast_elements

      | {value = `Heading (level, label, content); _} ->
        let parsed_a_title, element =
          section_heading
            status
            ~parsed_a_title
            ast_element.Location.location
            level
            label
            content
        in
        traverse ~parsed_a_title (element::comment_elements_acc) ast_elements
  in

  traverse ~parsed_a_title:false [] ast_elements



let ast_to_comment ~permissive ~sections_allowed ~parent_of_sections ast =
  let status =
    {
      permissive;
      warnings = [];
      sections_allowed;
      parent_of_sections;
    }
  in

  let result = Error.catch (fun () -> top_level_block_elements status ast) in
  let warnings = List.rev status.warnings in

  {Error.result; warnings}
