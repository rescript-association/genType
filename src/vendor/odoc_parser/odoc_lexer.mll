{

let unescape_word : string -> string = fun s ->
  (* The common case is that there are no escape sequences. *)
  match String.index s '\\' with
  | exception Not_found -> s
  | _ ->
    let buffer = Buffer.create (String.length s) in
    let rec scan_word index =
      if index >= String.length s then
        ()
      else
        let c = s.[index] in
        let c, increment =
          match c with
          | '\\' ->
            if index + 1 < String.length s then
              match s.[index + 1] with
              | '{' | '}' | '[' | ']' | '@' as c -> c, 2
              | _ -> c, 1
            else c, 1
          | _ -> c, 1
        in
        Buffer.add_char buffer c;
        scan_word (index + increment)
    in
    scan_word 0;
    Buffer.contents buffer



(* This is used for code and verbatim blocks. It can be done with a regular
   expression, but the regexp gets quite ugly, so a function is easier to
   understand. *)
let trim_leading_blank_lines : string -> string = fun s ->
  let rec scan_for_last_newline : int -> int -> int =
      fun index trim_until ->
    if index >= String.length s then
      String.length s
    else
      match s.[index] with
      | ' ' | '\t' | '\r' -> scan_for_last_newline (index + 1) trim_until
      | '\n' -> scan_for_last_newline (index + 1) (index + 1)
      | _ -> trim_until
  in
  let trim_until = scan_for_last_newline 0 0 in
  String.sub s trim_until (String.length s - trim_until)

let trim_trailing_blank_lines : string -> string = fun s ->
  let rec scan_for_last_newline : int -> int option -> int option =
      fun index trim_from ->
    if index < 0 then
      Some 0
    else
      match s.[index] with
      | ' ' | '\t' | '\r' -> scan_for_last_newline (index - 1) trim_from
      | '\n' -> scan_for_last_newline (index - 1) (Some index)
      | _ -> trim_from
  in
  let last = String.length s - 1 in
  match scan_for_last_newline last None with
  | None ->
    s
  | Some trim_from ->
    let trim_from =
      if trim_from > 0 && s.[trim_from - 1] = '\r' then
        trim_from - 1
      else
        trim_from
    in
    String.sub s 0 trim_from



module Location = Location_
module Error = Error



(* Assuming an ASCII-compatible input encoding here. *)
let heading_level level =
  Char.code level - Char.code '0'



type input = {
  file : string;
  offset_to_location : int -> Location.point;
  lexbuf : Lexing.lexbuf;
}

let offset_span_to_location
    ?start_offset ?adjust_start_by ?end_offset ?adjust_end_by input =
  let start =
    match start_offset with
    | None -> Lexing.lexeme_start input.lexbuf
    | Some s -> s
  in
  let start =
    match adjust_start_by with
    | None -> start
    | Some s -> start + String.length s
  in
  let end_ =
    match end_offset with
    | None -> Lexing.lexeme_end input.lexbuf
    | Some e -> e
  in
  let end_ =
    match adjust_end_by with
    | None -> end_
    | Some s -> end_ - String.length s
  in
  {
    Location_.file = input.file;
    start = input.offset_to_location start;
    end_ = input.offset_to_location end_;
  }

let emit input ?start_offset ?adjust_start_by ?adjust_end_by token =
  let location =
    offset_span_to_location
      ?start_offset ?adjust_start_by ?adjust_end_by input
  in
  Location.at location token

let raise_error
    input ?start_offset ?adjust_start_by ?end_offset ?adjust_end_by error =
  let location =
    offset_span_to_location
      ?start_offset ?adjust_start_by ?end_offset ?adjust_end_by input
  in
  Error.raise_exception (error location)

let reference_token start target =
  match start with
  | "{!" -> `Simple_reference target
  | "{{!" -> `Begin_reference_with_replacement_text target
  | "{:" -> `Simple_link target
  | "{{:" -> `Begin_link_with_replacement_text target
  | _ -> assert false

let emit_reference input start target =
  let target = String.trim target in
  let token = reference_token start target in
  if target = "" then
    raise_error input (Parse_error.cannot_be_empty ~what:(Token.describe token))
  else
    emit input token



let trim_leading_space_or_accept_whitespace input text =
  match text.[0] with
  | ' ' -> String.sub text 1 (String.length text - 1)
  | '\t' | '\r' | '\n' -> text
  | exception Invalid_argument _ -> ""
  | _ ->
    raise_error
      input
      ~end_offset:(Lexing.lexeme_start input.lexbuf + 2)
      Parse_error.no_leading_whitespace_in_verbatim

let trim_trailing_space_or_accept_whitespace input text =
  match text.[String.length text - 1] with
  | ' ' -> String.sub text 0 (String.length text - 1)
  | '\t' | '\r' | '\n' -> text
  | exception Invalid_argument _ -> ""
  | _ ->
    raise_error
      input
      ~start_offset:(Lexing.lexeme_end input.lexbuf - 2)
      Parse_error.no_trailing_whitespace_in_verbatim

}



let markup_char =
  ['{' '}' '[' ']' '@']
let space_char =
  [' ' '\t' '\n' '\r']
let bullet_char =
  ['-' '+']

let word_char =
  (_ # markup_char # space_char # bullet_char) | ('\\' markup_char)

let horizontal_space =
  [' ' '\t']
let newline =
  '\n' | "\r\n"

let reference_start =
  "{!" | "{{!" | "{:" | "{{:"

let code_block_text =
  ([^ ']'] | ']'+ [^ ']' '}'])* ']'*
let verbatim_text =
  ([^ 'v'] | 'v'+ [^ 'v' '}'])* 'v'*



rule token input = parse
  | horizontal_space* eof
    { emit input `End }

  | (horizontal_space* newline as prefix)
    horizontal_space* ((newline horizontal_space*)+ as suffix)
    { emit input `Blank_line ~adjust_start_by:prefix ~adjust_end_by:suffix }

  | horizontal_space* newline horizontal_space*
    { emit input `Single_newline }

  | horizontal_space+
    { emit input `Space }

  | (horizontal_space* (newline horizontal_space*)? as p) '}'
    { emit input `Right_brace ~adjust_start_by:p }

  | word_char (word_char | bullet_char | '@')*
  | bullet_char (word_char | bullet_char | '@')+ as w
    { emit input (`Word (unescape_word w)) }

  | '['
    { code_span
        (Buffer.create 1024) 0 (Lexing.lexeme_start lexbuf) input lexbuf }

  | '-'
    { emit input `Minus }

  | '+'
    { emit input `Plus }

  | "{b"
    { emit input (`Begin_style `Bold) }

  | "{i"
    { emit input (`Begin_style `Italic) }

  | "{e"
    { emit input (`Begin_style `Emphasis) }

  | "{^"
    { emit input (`Begin_style `Superscript) }

  | "{_"
    { emit input (`Begin_style `Subscript) }

  | "{!modules:" ([^ '}']* as modules) '}'
    { emit input (`Modules modules) }

  | (reference_start as start) ([^ '}']* as target) '}'
    { emit_reference input start target }

  | "{[" (code_block_text as c) "]}"
    { let c = trim_leading_blank_lines c in
      let c = trim_trailing_blank_lines c in
      emit input (`Code_block c) }

  | "{v" (verbatim_text as t) "v}"
    { let t = trim_leading_space_or_accept_whitespace input t in
      let t = trim_trailing_space_or_accept_whitespace input t in
      let t = trim_leading_blank_lines t in
      let t = trim_trailing_blank_lines t in
      emit input (`Verbatim t) }

  | "{ul"
    { emit input (`Begin_list `Unordered) }

  | "{ol"
    { emit input (`Begin_list `Ordered) }

  | "{li"
    { emit input (`Begin_list_item `Li) }

  | "{-"
    { emit input (`Begin_list_item `Dash) }

  | '{' (['0'-'9'] as level) ':' (([^ '}'] # space_char)+ as label)
    { emit input (`Begin_section_heading (heading_level level, Some label)) }

  | '{' (['0'-'9'] as level)
    { emit input (`Begin_section_heading (heading_level level, None)) }

  | "@example" horizontal_space* ([^ '{']* as lang) "{[" (code_block_text as content) "]}"
    { emit input (`Example (lang, content)) }

  | "@author" horizontal_space+ ([^ '\r' '\n']* as author)
    { emit input (`Tag (`Author author)) }

  | "@deprecated"
    { emit input (`Tag `Deprecated) }

  | "@doc" horizontal_space+ ([^ '\r' '\n']* as doc)
    { emit input (`Doc doc) }

  | "@param" horizontal_space+ ((_ # space_char)+ as name)
    { emit input (`Tag (`Param name)) }

  | "@raise" horizontal_space+ ((_ # space_char)+ as name)
    { emit input (`Tag (`Raise name)) }

  | "@return"
    { emit input (`Tag `Return) }

  | "@see" horizontal_space* '<' ([^ '>']* as url) '>'
    { emit input (`Tag (`See (`Url, url))) }

  | "@see" horizontal_space* '\'' ([^ '>']* as filename) '\''
    { emit input (`Tag (`See (`File, filename))) }

  | "@see" horizontal_space* '"' ([^ '>']* as name) '"'
    { emit input (`Tag (`See (`Document, name))) }

  | "@since" horizontal_space+ ([^ '\r' '\n']* as version)
    { emit input (`Tag (`Since version)) }

  | "@before" horizontal_space+ ((_ # space_char)+ as version)
    { emit input (`Tag (`Before version)) }

  | "@version" horizontal_space+ ([^ '\r' '\n']* as version)
    { emit input (`Tag (`Version version)) }

  | "@canonical" horizontal_space+ ([^ '\r' '\n']* as identifier)
    { emit input (`Tag (`Canonical identifier)) }

  | "@inline"
    { emit input (`Tag `Inline) }

  | "@open"
    { emit input (`Tag `Open) }

  | "@closed"
    { emit input (`Tag `Closed) }




  | '{' (['0'-'9'] ['0'-'9']+ as level)
    { raise_error input (Parse_error.bad_section_level level) }

  | ('{' ['0'-'9'] as prefix) ':'
    { raise_error
        input
        ~adjust_start_by:prefix
        (Parse_error.cannot_be_empty ~what:"heading label") }

  | '{' _? as markup
    { raise_error input (Parse_error.bad_markup markup) }

  | ']'
    { raise_error input Parse_error.unpaired_right_bracket }

  | '@' ("author" | "since" | "version" | "canonical")
    { raise_error
        input
        (Parse_error.cannot_be_empty
          ~what:(Printf.sprintf "'%s'" (Lexing.lexeme lexbuf))) }

  | "@param"
    { raise_error input Parse_error.truncated_param }

  | "@raise"
    { raise_error input Parse_error.truncated_raise }

  | "@before"
    { raise_error input Parse_error.truncated_before }

  | "@see"
    { raise_error input Parse_error.truncated_see }

  | '@' ['a'-'z' 'A'-'Z']+ as tag
    { raise_error input (Parse_error.unknown_tag tag) }

  | '@'
    { raise_error input Parse_error.stray_at }

  | '\r'
    { raise_error input Parse_error.stray_cr }

  | "{!modules:" [^ '}']* eof
    { raise_error
        input
        ~start_offset:(Lexing.lexeme_end lexbuf)
        (Parse_error.not_allowed
          ~what:(Token.describe `End)
          ~in_what:(Token.describe (`Modules ""))) }

  | (reference_start as start) [^ '}']* eof
    { raise_error
        input
        ~start_offset:(Lexing.lexeme_end lexbuf)
        (Parse_error.not_allowed
          ~what:(Token.describe `End)
          ~in_what:(Token.describe (reference_token start ""))) }

  | "{[" code_block_text eof
    { raise_error
        input
        ~start_offset:(Lexing.lexeme_end lexbuf)
        (Parse_error.not_allowed
          ~what:(Token.describe `End)
          ~in_what:(Token.describe (`Code_block ""))) }

  | "{v" verbatim_text eof
    { raise_error
        input
        ~start_offset:(Lexing.lexeme_end lexbuf)
        (Parse_error.not_allowed
          ~what:(Token.describe `End)
          ~in_what:(Token.describe (`Verbatim ""))) }



and code_span buffer nesting_level start_offset input = parse
  | ']'
    { if nesting_level = 0 then
        emit input (`Code_span (Buffer.contents buffer)) ~start_offset
      else begin
        Buffer.add_char buffer ']';
        code_span buffer (nesting_level - 1) start_offset input lexbuf
      end }

  | '['
    { Buffer.add_char buffer '[';
      code_span buffer (nesting_level + 1) start_offset input lexbuf }

  | '\\' ('[' | ']' as c)
    { Buffer.add_char buffer c;
      code_span buffer nesting_level start_offset input lexbuf }

  | newline newline
    { raise_error
        input
        (Parse_error.not_allowed
          ~what:(Token.describe `Blank_line)
          ~in_what:(Token.describe (`Code_span ""))) }

  | eof
    { raise_error
        input
        (Parse_error.not_allowed
          ~what:(Token.describe `End)
          ~in_what:(Token.describe (`Code_span ""))) }

  | _ as c
    { Buffer.add_char buffer c;
      code_span buffer nesting_level start_offset input lexbuf }
