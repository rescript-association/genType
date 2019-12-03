# 18 "parsing/lexer.mll"
 
open Lexing
open Misc
open Parser

type directive_value =
  | Dir_bool of bool 
  | Dir_float of float
  | Dir_int of int
  | Dir_string of string
  | Dir_null 

type directive_type = 
  | Dir_type_bool 
  | Dir_type_float 
  | Dir_type_int 
  | Dir_type_string 
  | Dir_type_null 

let type_of_directive x =
  match x with 
  | Dir_bool _ -> Dir_type_bool
  | Dir_float _ -> Dir_type_float
  | Dir_int _ -> Dir_type_int
  | Dir_string _ -> Dir_type_string
  | Dir_null -> Dir_type_null

let string_of_type_directive x = 
  match x with 
  | Dir_type_bool  -> "bool"
  | Dir_type_float  -> "float"
  | Dir_type_int  -> "int"
  | Dir_type_string  -> "string"
  | Dir_type_null -> "null"

type error =
  | Illegal_character of char
  | Illegal_escape of string
  | Unterminated_comment of Location.t
  | Unterminated_string
  | Unterminated_string_in_comment of Location.t * Location.t
  | Keyword_as_label of string
  | Invalid_literal of string
  | Invalid_directive of string * string option
  | Unterminated_paren_in_conditional
  | Unterminated_if
  | Unterminated_else 
  | Unexpected_token_in_conditional 
  | Expect_hash_then_in_conditional
  | Illegal_semver of string
  | Unexpected_directive 
  | Conditional_expr_expected_type of directive_type * directive_type
                           
;;

exception Error of error * Location.t;;

let assert_same_type  lexbuf x y = 
  let lhs = type_of_directive x in let rhs =  type_of_directive y  in
  if lhs <> rhs then 
    raise (Error(Conditional_expr_expected_type(lhs,rhs), Location.curr lexbuf))
  else y

let directive_built_in_values  =
  Hashtbl.create 51


let replace_directive_built_in_value k v = 
  Hashtbl.replace directive_built_in_values k v 

let remove_directive_built_in_value k  = 
  Hashtbl.replace directive_built_in_values k Dir_null

let replace_directive_int k v = 
  Hashtbl.replace directive_built_in_values k (Dir_int v)

let replace_directive_bool k v = 
  Hashtbl.replace directive_built_in_values k (Dir_bool v)

let replace_directive_string k v = 
  Hashtbl.replace directive_built_in_values k (Dir_string v)

let () =
  (* Note we use {!Config} instead of {!Sys} becasue 
     we want to overwrite in some cases with the 
     same stdlib
  *)
  let version = 
    Config.version (* so that it can be overridden*)
  in
  replace_directive_built_in_value "OCAML_VERSION" 
    (Dir_string version);
  replace_directive_built_in_value "OCAML_PATCH"
    (Dir_string 
       (match String.rindex version '+' with 
       | exception Not_found -> ""
       | i -> 
           String.sub version (i + 1)
             (String.length version - i - 1)))
  ;
  replace_directive_built_in_value "OS_TYPE" 
    (Dir_string Sys.os_type);
  replace_directive_built_in_value "BIG_ENDIAN" 
    (Dir_bool Sys.big_endian);
  replace_directive_built_in_value "WORD_SIZE" 
    (Dir_int Sys.word_size)

let find_directive_built_in_value k =
  Hashtbl.find directive_built_in_values k 

let iter_directive_built_in_value f = Hashtbl.iter f directive_built_in_values

(*
   {[
     # semver 0 "12";;
     - : int * int * int * string = (12, 0, 0, "");;
     # semver 0 "12.3";;
     - : int * int * int * string = (12, 3, 0, "");;
       semver 0 "12.3.10";;
     - : int * int * int * string = (12, 3, 10, "");;
     # semver 0 "12.3.10+x";;
     - : int * int * int * string = (12, 3, 10, "+x")
   ]}
*)    
let zero = Char.code '0' 
let dot = Char.code '.'
let semantic_version_parse str start  last_index = 
  let rec aux start  acc last_index =
    if start <= last_index then
      let c = Char.code (String.unsafe_get str start) in
      if c = dot then (acc, start + 1) (* consume [4.] instead of [4]*)
      else 
        let v =  c - zero in
        if v >=0 && v <= 9  then
          aux (start + 1) (acc * 10 + v) last_index
        else (acc , start)
    else (acc, start)
  in
  let major, major_end =  aux start 0 last_index  in
  let minor, minor_end = aux major_end 0 last_index in
  let patch, patch_end = aux minor_end 0 last_index in 
  let additional = String.sub str patch_end (last_index - patch_end  +1) in
  (major, minor, patch), additional

(** 
   {[
     semver Location.none "1.2.3" "~1.3.0" = false;;
     semver Location.none "1.2.3" "^1.3.0" = true ;;
     semver Location.none "1.2.3" ">1.3.0" = false ;;
     semver Location.none "1.2.3" ">=1.3.0" = false ;;
     semver Location.none "1.2.3" "<1.3.0" = true ;;
     semver Location.none "1.2.3" "<=1.3.0" = true ;;
   ]}
*)
let semver loc lhs str =
  let last_index = String.length str - 1 in 
  if last_index < 0 then raise (Error(Illegal_semver str, loc))
  else 
    let pred, ((major, minor, _patch) as version, _) = 
      let v = String.unsafe_get str 0 in 
      match v with
      | '>' -> 
          if last_index = 0 then raise (Error(Illegal_semver str, loc)) else 
          if String.unsafe_get str 1 = '=' then 
            `Ge, semantic_version_parse str 2 last_index
          else `Gt, semantic_version_parse str 1 last_index
      | '<' 
        ->
          if last_index = 0 then raise (Error(Illegal_semver str, loc)) else 
          if String.unsafe_get str 1 = '=' then 
            `Le, semantic_version_parse str 2 last_index
          else `Lt, semantic_version_parse str 1 last_index
      | '^' 
        -> `Compatible, semantic_version_parse str 1 last_index
      | '~' ->  `Approximate, semantic_version_parse str 1 last_index
      | _ -> `Exact, semantic_version_parse str 0 last_index 
    in 
    let ((l_major, l_minor, _l_patch) as lversion,_) =
      semantic_version_parse lhs 0 (String.length lhs - 1) in 
    match pred with 
    | `Ge -> lversion >= version 
    | `Gt -> lversion > version 
    | `Le -> lversion <= version
    | `Lt -> lversion < version 
    | `Approximate -> major = l_major && minor = l_minor 
    |  `Compatible -> major = l_major
    | `Exact -> lversion = version 


let pp_directive_value fmt (x : directive_value) =
  match x with
  | Dir_bool b -> Format.pp_print_bool fmt b
  | Dir_int b -> Format.pp_print_int fmt b
  | Dir_float b -> Format.pp_print_float fmt b
  | Dir_string s  -> Format.fprintf fmt "%S" s
  | Dir_null -> Format.pp_print_string fmt "null"    

let list_variables fmt = 
  iter_directive_built_in_value 
    (fun s  dir_value ->
       Format.fprintf
         fmt "@[%s@ %a@]@."
         s pp_directive_value dir_value
    )

let defined str =
  begin match  find_directive_built_in_value str with 
  |  Dir_null -> false 
  | _ ->  true
  | exception _ -> 
      try ignore @@ Sys.getenv str; true with _ ->  false 
  end

let query _loc str =
  begin match find_directive_built_in_value str with
  | Dir_null -> Dir_bool false
  | v -> v
  | exception Not_found ->
      begin match Sys.getenv str with 
      | v -> 
          begin 
            try Dir_bool (bool_of_string v) with 
              _ -> 
                begin 
                  try Dir_int (int_of_string v )
                  with 
                    _ -> 
                      begin try (Dir_float (float_of_string v)) 
                      with _ -> Dir_string v
                      end
                end
          end
      | exception Not_found -> 
          Dir_bool false
      end
  end


let define_key_value key v  =
  if String.length key > 0
      && Char.uppercase_ascii (key.[0]) = key.[0] then 
    begin 
      replace_directive_built_in_value key
      begin
        (* NEED Sync up across {!lexer.mll} {!bspp.ml} and here,
           TODO: put it in {!lexer.mll}
        *)
        try Dir_bool (bool_of_string v) with 
          _ -> 
          begin 
            try Dir_int (int_of_string v )
            with 
              _ -> 
              begin try (Dir_float (float_of_string v)) 
                with _ -> Dir_string v
              end
          end
      end;
    true
    end
  else false 

let cvt_int_literal s =
  - int_of_string ("-" ^ s)
  
let value_of_token loc (t : Parser.token)  = 
  match t with 
  | INT (i,None) -> Dir_int (cvt_int_literal i) 
  | STRING (s,_) -> Dir_string s 
  | FLOAT (s,None)  -> Dir_float (float_of_string s)
  | TRUE -> Dir_bool true
  | FALSE -> Dir_bool false
  | UIDENT s -> query loc s 
  | _ -> raise (Error (Unexpected_token_in_conditional, loc))


let directive_parse token_with_comments lexbuf =
  let look_ahead = ref None in
  let token () : Parser.token =
    let v = !look_ahead in
    match v with 
    | Some v -> 
        look_ahead := None ;
        v
    | None ->
       let rec skip () = 
        match token_with_comments lexbuf  with
        | COMMENT _ 
        | DOCSTRING _ 
        | EOL -> skip ()
        | EOF -> raise (Error (Unterminated_if, Location.curr lexbuf)) 
        | t -> t 
        in  skip ()
  in
  let push e =
    (* INVARIANT: only look at most one token *)
    assert (!look_ahead = None);
    look_ahead := Some e 
  in
  let rec
    token_op calc   ~no  lhs   =
    match token () with 
    | (LESS 
    | GREATER 
    | INFIXOP0 "<=" 
    | INFIXOP0 ">=" 
    | EQUAL
    | INFIXOP0 "<>" as op) ->
        let f =  
          match op with 
          | LESS -> (<) 
          | GREATER -> (>)
          | INFIXOP0 "<=" -> (<=)
          | EQUAL -> (=)
          | INFIXOP0 "<>" -> (<>) 
          | _ -> assert false
        in 
        let curr_loc = Location.curr lexbuf in 
        let rhs = value_of_token curr_loc (token ()) in 
        not calc ||
        f lhs (assert_same_type lexbuf lhs rhs)
    | INFIXOP0 "=~" -> 
        not calc ||
        begin match lhs with 
        | Dir_string s ->
            let curr_loc = Location.curr lexbuf in 
            let rhs = value_of_token curr_loc (token ()) in 
            begin match rhs with 
            | Dir_string rhs -> 
                semver curr_loc s rhs
            | _ -> 
                raise
                  (Error
                     ( Conditional_expr_expected_type
                         (Dir_type_string, type_of_directive lhs), Location.curr lexbuf))
            end
        | _ -> raise
                 (Error
                    ( Conditional_expr_expected_type
                        (Dir_type_string, type_of_directive lhs), Location.curr lexbuf))
        end
    | e -> no e 
  and
    parse_or calc : bool =
    parse_or_aux calc (parse_and calc)
  and  (* a || (b || (c || d))*)
    parse_or_aux calc v : bool =
    (* let l = v  in *)
    match token () with
    | BARBAR ->
        let b =   parse_or (calc && not v)  in
        v || b 
    | e -> push e ; v
  and parse_and calc = 
    parse_and_aux calc (parse_relation calc)
  and parse_and_aux calc v = (* a && (b && (c && d)) *)
    (* let l = v  in *)
    match token () with
    | AMPERAMPER ->
        let b =  parse_and (calc && v) in
        v && b
    | e -> push e ; v
  and parse_relation (calc : bool) : bool  =
    let curr_token = token () in
    let curr_loc = Location.curr lexbuf in
    match curr_token with
    | TRUE -> true 
    | FALSE -> false
    | UIDENT v ->
        let value_v = query curr_loc v in
        token_op calc 
          ~no:(fun e -> push e ;
                match value_v with 
                | Dir_bool b -> b 
                | _ -> 
                    let ty = type_of_directive value_v in
                    raise
                      (Error(Conditional_expr_expected_type (Dir_type_bool, ty),
                             curr_loc)))
          value_v
    | INT (v,None) -> 
      let num_v = cvt_int_literal v in 
      token_op calc
          ~no:(fun e -> 
                push e; 
                num_v <> 0
              )
          (Dir_int num_v)
    | FLOAT (v,None) -> 
        token_op calc
          ~no:(fun _e -> 
              raise (Error(Conditional_expr_expected_type(Dir_type_bool, Dir_type_float),
                           curr_loc)))
          (Dir_float (float_of_string v))
    | STRING (v,_) -> 
        token_op calc
          ~no:(fun _e ->
              raise (Error
                       (Conditional_expr_expected_type(Dir_type_bool, Dir_type_string),
                        curr_loc)))
          (Dir_string v)
    | LIDENT ("defined" | "undefined" as r) ->
        let t = token () in 
        let loc = Location.curr lexbuf in
        begin match t with
        | UIDENT s -> 
            not calc || 
            if r.[0] = 'u' then 
              not @@ defined s
            else defined s 
        | _ -> raise (Error (Unexpected_token_in_conditional, loc))
        end
    | LPAREN ->
        let v = parse_or calc in
        begin match token () with
        | RPAREN ->  v
        | _ -> raise (Error(Unterminated_paren_in_conditional, Location.curr lexbuf))
        end 

    | _ -> raise (Error (Unexpected_token_in_conditional, curr_loc))
  in
  let v = parse_or true in
  begin match token () with
  | THEN ->  v 
  | _ -> raise (Error (Expect_hash_then_in_conditional, Location.curr lexbuf))
  end


type dir_conditional =
  | Dir_if_true
  | Dir_if_false
  | Dir_out 

(* let string_of_dir_conditional (x : dir_conditional) = *)
(*   match x with  *)
(*   | Dir_if_true -> "Dir_if_true" *)
(*   | Dir_if_false -> "Dir_if_false" *)
(*   | Dir_out -> "Dir_out" *)

let is_elif (i : Parser.token ) =
  match i with
  | LIDENT "elif" -> true
  | _ -> false (* avoid polymorphic equal *)


(* The table of keywords *)

let keyword_table =
  create_hashtable 149 [
    "and", AND;
    "as", AS;
    "assert", ASSERT;
    "begin", BEGIN;
    "class", CLASS;
    "constraint", CONSTRAINT;
    "do", DO;
    "done", DONE;
    "downto", DOWNTO;
    "else", ELSE;
    "end", END;
    "exception", EXCEPTION;
    "external", EXTERNAL;
    "false", FALSE;
    "for", FOR;
    "fun", FUN;
    "function", FUNCTION;
    "functor", FUNCTOR;
    "if", IF;
    "in", IN;
    "include", INCLUDE;
    "inherit", INHERIT;
    "initializer", INITIALIZER;
    "lazy", LAZY;
    "let", LET;
    "match", MATCH;
    "method", METHOD;
    "module", MODULE;
    "mutable", MUTABLE;
    "new", NEW;
    "nonrec", NONREC;
    "object", OBJECT;
    "of", OF;
    "open", OPEN;
    "or", OR;
(*  "parser", PARSER; *)
    "private", PRIVATE;
    "rec", REC;
    "sig", SIG;
    "struct", STRUCT;
    "then", THEN;
    "to", TO;
    "true", TRUE;
    "try", TRY;
    "type", TYPE;
    "val", VAL;
    "virtual", VIRTUAL;
    "when", WHEN;
    "while", WHILE;
    "with", WITH;

    "lor", INFIXOP3("lor"); (* Should be INFIXOP2 *)
    "lxor", INFIXOP3("lxor"); (* Should be INFIXOP2 *)
    "mod", INFIXOP3("mod");
    "land", INFIXOP3("land");
    "lsl", INFIXOP4("lsl");
    "lsr", INFIXOP4("lsr");
    "asr", INFIXOP4("asr")
]

(* To buffer string literals *)

let string_buffer = Buffer.create 256
let reset_string_buffer () = Buffer.reset string_buffer
let get_stored_string () = Buffer.contents string_buffer

let store_string_char c = Buffer.add_char string_buffer c
let store_string_utf_8_uchar u = Buffer.add_utf_8_uchar string_buffer u
let store_string s = Buffer.add_string string_buffer s
let store_lexeme lexbuf = store_string (Lexing.lexeme lexbuf)

(* To store the position of the beginning of a string and comment *)
let string_start_loc = ref Location.none;;
let comment_start_loc = ref [];;
let in_comment () = !comment_start_loc <> [];;
let is_in_string = ref false
let in_string () = !is_in_string
let print_warnings = ref true
let if_then_else = ref Dir_out
let sharp_look_ahead = ref None
let update_if_then_else v = 
  (* Format.fprintf Format.err_formatter "@[update %s \n@]@." (string_of_dir_conditional v); *)
  if_then_else := v

(* Escaped chars are interpreted in strings unless they are in comments. *)
let store_escaped_char lexbuf c =
  if in_comment () then store_lexeme lexbuf else store_string_char c

let store_escaped_uchar lexbuf u =
  if in_comment () then store_lexeme lexbuf else store_string_utf_8_uchar u

let with_comment_buffer comment lexbuf =
  let start_loc = Location.curr lexbuf  in
  comment_start_loc := [start_loc];
  reset_string_buffer ();
  let end_loc = comment lexbuf in
  let s = get_stored_string () in
  reset_string_buffer ();
  let loc = { start_loc with Location.loc_end = end_loc.Location.loc_end } in
  s, loc

(* To translate escape sequences *)

let hex_digit_value d = (* assert (d in '0'..'9' 'a'..'f' 'A'..'F') *)
  let d = Char.code d in
  if d >= 97 then d - 87 else
  if d >= 65 then d - 55 else
  d - 48

let hex_num_value lexbuf ~first ~last =
  let rec loop acc i = match i > last with
  | true -> acc
  | false ->
      let value = hex_digit_value (Lexing.lexeme_char lexbuf i) in
      loop (16 * acc + value) (i + 1)
  in
  loop 0 first

let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c

let char_for_decimal_code lexbuf i =
  let c = 100 * (Char.code(Lexing.lexeme_char lexbuf i) - 48) +
           10 * (Char.code(Lexing.lexeme_char lexbuf (i+1)) - 48) +
                (Char.code(Lexing.lexeme_char lexbuf (i+2)) - 48) in
  if (c < 0 || c > 255) then
    if in_comment ()
    then 'x'
    else raise (Error(Illegal_escape (Lexing.lexeme lexbuf),
                      Location.curr lexbuf))
  else Char.chr c

let char_for_octal_code lexbuf i =
  let c = 64 * (Char.code(Lexing.lexeme_char lexbuf i) - 48) +
           8 * (Char.code(Lexing.lexeme_char lexbuf (i+1)) - 48) +
               (Char.code(Lexing.lexeme_char lexbuf (i+2)) - 48) in
  Char.chr c

let char_for_hexadecimal_code lexbuf i =
  let byte = hex_num_value lexbuf ~first:i ~last:(i+1) in
  Char.chr byte

let uchar_for_uchar_escape lexbuf =
  let err e =
    raise
      (Error (Illegal_escape (Lexing.lexeme lexbuf ^ e), Location.curr lexbuf))
  in
  let len = Lexing.lexeme_end lexbuf - Lexing.lexeme_start lexbuf in
  let first = 3 (* skip opening \u{ *) in
  let last = len - 2 (* skip closing } *) in
  let digit_count = last - first + 1 in
  match digit_count > 6 with
  | true -> err ", too many digits, expected 1 to 6 hexadecimal digits"
  | false ->
      let cp = hex_num_value lexbuf ~first ~last in
      if Uchar.is_valid cp then Uchar.unsafe_of_int cp else
      err (", " ^ Printf.sprintf "%X" cp ^ " is not a Unicode scalar value")

(* recover the name from a LABEL or OPTLABEL token *)

let get_label_name lexbuf =
  let s = Lexing.lexeme lexbuf in
  let name = String.sub s 1 (String.length s - 2) in
  if Hashtbl.mem keyword_table name then
    raise (Error(Keyword_as_label name, Location.curr lexbuf));
  name
;;

(* Update the current location with file name and line number. *)

let update_loc lexbuf file line absolute chars =
  let pos = lexbuf.lex_curr_p in
  let new_file = match file with
                 | None -> pos.pos_fname
                 | Some s -> s
  in
  lexbuf.lex_curr_p <- { pos with
    pos_fname = new_file;
    pos_lnum = if absolute then line else pos.pos_lnum + line;
    pos_bol = pos.pos_cnum - chars;
  }
;;

let preprocessor = ref None

let escaped_newlines = ref false

(* Warn about Latin-1 characters used in idents *)

let warn_latin1 lexbuf =
  Location.deprecated (Location.curr lexbuf)"ISO-Latin1 characters in identifiers"

let handle_docstrings = ref true
let comment_list = ref []

let add_comment com =
  comment_list := com :: !comment_list

let add_docstring_comment ds =
  let com =
    ("*" ^ Docstrings.docstring_body ds, Docstrings.docstring_loc ds)
  in
    add_comment com

let comments () = List.rev !comment_list

(* Error report *)

open Format

let report_error ppf = function
  | Illegal_character c ->
      fprintf ppf "Illegal character (%s)" (Char.escaped c)
  | Illegal_escape s ->
      fprintf ppf "Illegal backslash escape in string or character (%s)" s
  | Unterminated_comment _ ->
      fprintf ppf "Comment not terminated"
  | Unterminated_string ->
      fprintf ppf "String literal not terminated"
  | Unterminated_string_in_comment (_, loc) ->
      fprintf ppf "This comment contains an unterminated string literal@.\
                   %aString literal begins here"
              Location.print_error loc
  | Keyword_as_label kwd ->
      fprintf ppf "`%s' is a keyword, it cannot be used as label name" kwd
  | Invalid_literal s ->
      fprintf ppf "Invalid literal %s" s
  | Invalid_directive (dir, explanation) ->
      fprintf ppf "Invalid lexer directive %S" dir;
      begin match explanation with
        | None -> ()
        | Some expl -> fprintf ppf ": %s" expl
      end
  | Unterminated_if -> 
      fprintf ppf "#if not terminated"
  | Unterminated_else -> 
      fprintf ppf "#else not terminated"
  | Unexpected_directive -> fprintf ppf "Unexpected directive"
  | Unexpected_token_in_conditional -> 
      fprintf ppf "Unexpected token in conditional predicate"
  | Unterminated_paren_in_conditional ->
    fprintf ppf "Unterminated parens in conditional predicate"
  | Expect_hash_then_in_conditional -> 
      fprintf ppf "Expect `then` after conditional predicate"
  | Conditional_expr_expected_type (a,b) -> 
      fprintf ppf "Conditional expression type mismatch (%s,%s)" 
        (string_of_type_directive a )
        (string_of_type_directive b )
  | Illegal_semver s -> 
      fprintf ppf "Illegal semantic version string %s" s

let () =
  Location.register_error_of_exn
    (function
      | Error (err, loc) ->
          Some (Location.error_of_printer loc report_error err)
      | _ ->
          None
    )


# 717 "parsing/lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\162\255\163\255\224\000\003\001\038\001\073\001\108\001\
    \143\001\186\255\178\001\215\001\194\255\091\000\252\001\031\002\
    \068\000\071\000\065\002\100\002\212\255\214\255\217\255\135\002\
    \230\002\009\003\088\000\255\000\039\003\236\255\123\003\207\003\
    \035\004\243\004\195\005\147\006\114\007\206\007\158\008\122\000\
    \254\255\001\000\005\000\255\255\006\000\007\000\125\009\155\009\
    \107\010\250\255\249\255\059\011\011\012\247\255\246\255\219\012\
    \047\013\131\013\215\013\043\014\127\014\211\014\039\015\123\015\
    \207\015\035\016\087\000\119\016\203\016\031\017\115\017\199\017\
    \108\000\192\255\235\255\007\003\034\018\106\000\107\000\011\000\
    \234\255\233\255\228\255\152\002\099\000\118\000\113\000\232\255\
    \128\000\147\000\231\255\224\000\003\001\148\000\230\255\110\004\
    \149\000\229\255\148\000\224\255\217\000\223\255\222\000\034\018\
    \222\255\073\018\101\005\009\003\221\255\012\000\014\001\080\001\
    \115\001\024\001\221\255\013\000\119\018\158\018\193\018\231\018\
    \010\019\209\255\204\255\205\255\206\255\202\255\045\019\154\000\
    \183\000\195\255\196\255\197\255\217\000\182\255\180\255\189\255\
    \080\019\185\255\187\255\115\019\150\019\185\019\220\019\130\005\
    \243\255\244\255\017\000\245\255\174\001\223\005\253\255\248\000\
    \249\000\255\255\254\255\252\255\005\006\238\019\003\001\004\001\
    \018\000\251\255\250\255\249\255\222\006\026\003\005\001\248\255\
    \036\003\008\001\247\255\066\008\020\001\246\255\059\001\234\001\
    \245\255\246\255\247\255\060\001\055\020\255\255\248\255\193\000\
    \233\008\038\001\133\004\253\255\073\001\094\001\113\001\143\004\
    \252\255\192\002\027\004\251\255\230\009\250\255\182\010\089\020\
    \249\255\129\001\130\001\252\255\085\007\254\255\255\255\146\001\
    \147\001\253\255\177\007\033\001\044\001\148\001\151\001\045\001\
    \153\001\044\001\019\000\255\255";
  Lexing.lex_backtrk =
   "\255\255\255\255\255\255\090\000\089\000\086\000\085\000\078\000\
    \076\000\255\255\067\000\064\000\255\255\057\000\056\000\054\000\
    \052\000\048\000\045\000\081\000\255\255\255\255\255\255\036\000\
    \035\000\042\000\040\000\039\000\062\000\255\255\014\000\014\000\
    \013\000\012\000\011\000\010\000\007\000\004\000\003\000\002\000\
    \255\255\093\000\093\000\255\255\255\255\255\255\084\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\018\000\
    \018\000\016\000\015\000\018\000\015\000\015\000\014\000\016\000\
    \015\000\016\000\255\255\017\000\017\000\014\000\014\000\016\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\027\000\027\000\027\000\027\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\028\000\255\255\029\000\255\255\030\000\088\000\
    \255\255\091\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\037\000\087\000\082\000\044\000\
    \047\000\255\255\255\255\255\255\255\255\255\255\055\000\074\000\
    \071\000\255\255\255\255\255\255\072\000\255\255\255\255\255\255\
    \065\000\255\255\255\255\083\000\077\000\080\000\079\000\255\255\
    \255\255\255\255\012\000\255\255\012\000\012\000\255\255\012\000\
    \012\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\010\000\010\000\255\255\255\255\007\000\
    \007\000\007\000\007\000\255\255\001\000\007\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\003\000\255\255\255\255\003\000\
    \255\255\255\255\255\255\002\000\255\255\255\255\001\000\255\255\
    \255\255\255\255\255\255\255\255";
  Lexing.lex_default =
   "\001\000\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\255\255\255\255\000\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\000\000\000\000\000\000\255\255\
    \255\255\255\255\255\255\077\000\255\255\000\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\000\000\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\255\255\255\255\000\000\000\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\255\255\082\000\255\255\255\255\255\255\
    \000\000\000\000\000\000\255\255\255\255\255\255\255\255\000\000\
    \255\255\255\255\000\000\255\255\255\255\255\255\000\000\255\255\
    \255\255\000\000\255\255\000\000\255\255\000\000\255\255\255\255\
    \000\000\255\255\110\000\255\255\000\000\255\255\110\000\111\000\
    \110\000\113\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\000\000\000\000\000\000\255\255\255\255\
    \255\255\000\000\000\000\000\000\255\255\000\000\000\000\000\000\
    \255\255\000\000\000\000\255\255\255\255\255\255\255\255\144\000\
    \000\000\000\000\255\255\000\000\158\000\255\255\000\000\255\255\
    \255\255\000\000\000\000\000\000\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\000\000\255\255\255\255\255\255\000\000\
    \255\255\255\255\000\000\255\255\255\255\000\000\255\255\176\000\
    \000\000\000\000\000\000\255\255\182\000\000\000\000\000\255\255\
    \255\255\255\255\255\255\000\000\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\000\000\255\255\000\000\255\255\255\255\
    \000\000\255\255\203\000\000\000\255\255\000\000\000\000\255\255\
    \255\255\000\000\255\255\255\255\255\255\213\000\216\000\255\255\
    \216\000\255\255\255\255\000\000";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\039\000\040\000\040\000\039\000\041\000\045\000\043\000\
    \043\000\040\000\044\000\044\000\045\000\078\000\108\000\114\000\
    \079\000\109\000\115\000\145\000\159\000\219\000\174\000\160\000\
    \039\000\008\000\029\000\024\000\006\000\004\000\023\000\027\000\
    \026\000\021\000\025\000\007\000\020\000\019\000\018\000\003\000\
    \031\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\017\000\016\000\015\000\014\000\010\000\036\000\
    \005\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\013\000\042\000\012\000\005\000\038\000\
    \022\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\028\000\011\000\009\000\037\000\125\000\
    \127\000\124\000\098\000\039\000\123\000\122\000\039\000\065\000\
    \065\000\065\000\065\000\065\000\065\000\065\000\065\000\065\000\
    \065\000\081\000\080\000\091\000\091\000\091\000\091\000\130\000\
    \087\000\129\000\039\000\128\000\071\000\071\000\071\000\071\000\
    \071\000\071\000\071\000\071\000\071\000\071\000\088\000\088\000\
    \088\000\088\000\088\000\088\000\088\000\088\000\088\000\088\000\
    \089\000\089\000\089\000\089\000\089\000\089\000\089\000\089\000\
    \089\000\089\000\090\000\094\000\097\000\099\000\100\000\134\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\131\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\132\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \002\000\003\000\101\000\102\000\003\000\003\000\003\000\101\000\
    \102\000\078\000\003\000\003\000\079\000\003\000\003\000\003\000\
    \092\000\092\000\092\000\092\000\092\000\092\000\092\000\092\000\
    \108\000\133\000\003\000\109\000\003\000\003\000\003\000\003\000\
    \003\000\154\000\114\000\153\000\003\000\115\000\255\255\003\000\
    \003\000\003\000\163\000\162\000\167\000\003\000\003\000\170\000\
    \003\000\003\000\003\000\093\000\093\000\093\000\093\000\093\000\
    \093\000\093\000\093\000\173\000\198\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\212\000\145\000\178\000\005\000\
    \174\000\201\000\005\000\005\000\005\000\213\000\217\000\218\000\
    \005\000\005\000\188\000\005\000\005\000\005\000\193\000\193\000\
    \193\000\193\000\108\000\076\000\003\000\109\000\003\000\000\000\
    \005\000\003\000\005\000\005\000\005\000\005\000\005\000\000\000\
    \188\000\188\000\006\000\190\000\000\000\006\000\006\000\006\000\
    \000\000\000\000\113\000\006\000\006\000\000\000\006\000\006\000\
    \006\000\000\000\000\000\188\000\112\000\108\000\190\000\003\000\
    \109\000\003\000\000\000\006\000\005\000\006\000\006\000\006\000\
    \006\000\006\000\000\000\178\000\206\000\117\000\201\000\207\000\
    \117\000\117\000\117\000\112\000\000\000\111\000\117\000\117\000\
    \000\000\117\000\142\000\117\000\206\000\206\000\214\000\208\000\
    \208\000\215\000\005\000\215\000\005\000\000\000\117\000\006\000\
    \117\000\141\000\117\000\117\000\117\000\000\000\000\000\000\000\
    \139\000\000\000\000\000\139\000\139\000\139\000\000\000\000\000\
    \159\000\139\000\139\000\160\000\139\000\139\000\139\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\006\000\000\000\006\000\
    \000\000\139\000\117\000\139\000\140\000\139\000\139\000\139\000\
    \000\000\000\000\000\000\006\000\000\000\161\000\006\000\006\000\
    \006\000\000\000\000\000\000\000\006\000\006\000\000\000\006\000\
    \006\000\006\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \117\000\000\000\117\000\000\000\006\000\139\000\006\000\006\000\
    \006\000\006\000\006\000\000\000\178\000\000\000\000\000\179\000\
    \006\000\000\000\000\000\006\000\006\000\006\000\204\000\255\255\
    \000\000\006\000\006\000\000\000\006\000\006\000\006\000\000\000\
    \000\000\000\000\157\000\139\000\181\000\139\000\255\255\138\000\
    \006\000\006\000\000\000\006\000\006\000\006\000\006\000\006\000\
    \255\255\000\000\000\000\000\000\000\000\006\000\000\000\000\000\
    \006\000\006\000\006\000\000\000\000\000\000\000\006\000\006\000\
    \000\000\006\000\006\000\006\000\000\000\000\000\006\000\137\000\
    \006\000\000\000\000\000\000\000\135\000\006\000\006\000\000\000\
    \006\000\006\000\006\000\006\000\006\000\000\000\000\000\000\000\
    \006\000\000\000\000\000\006\000\006\000\006\000\180\000\000\000\
    \000\000\006\000\006\000\000\000\126\000\006\000\006\000\000\000\
    \255\255\000\000\000\000\136\000\000\000\006\000\000\000\000\000\
    \000\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \000\000\000\000\120\000\000\000\000\000\120\000\120\000\120\000\
    \000\000\000\000\000\000\120\000\120\000\000\000\120\000\121\000\
    \120\000\000\000\000\000\255\255\000\000\000\000\000\000\000\000\
    \006\000\000\000\006\000\120\000\000\000\006\000\120\000\120\000\
    \120\000\120\000\205\000\000\000\000\000\117\000\000\000\000\000\
    \117\000\117\000\117\000\000\000\000\000\000\000\117\000\117\000\
    \000\000\117\000\118\000\117\000\255\255\000\000\000\000\255\255\
    \000\000\255\255\000\000\006\000\000\000\006\000\117\000\120\000\
    \117\000\117\000\119\000\117\000\117\000\000\000\000\000\000\000\
    \006\000\000\000\000\000\006\000\006\000\116\000\255\255\000\000\
    \000\000\006\000\006\000\000\000\006\000\006\000\006\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\120\000\000\000\120\000\
    \000\000\006\000\117\000\006\000\006\000\006\000\006\000\006\000\
    \095\000\095\000\095\000\095\000\095\000\095\000\095\000\095\000\
    \095\000\095\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\095\000\095\000\095\000\095\000\095\000\095\000\000\000\
    \117\000\000\000\117\000\000\000\000\000\006\000\000\000\000\000\
    \000\000\000\000\177\000\000\000\000\000\000\000\000\000\107\000\
    \194\000\194\000\194\000\194\000\194\000\194\000\194\000\194\000\
    \000\000\095\000\095\000\095\000\095\000\095\000\095\000\000\000\
    \000\000\000\000\000\000\006\000\000\000\006\000\107\000\105\000\
    \000\000\105\000\105\000\105\000\105\000\000\000\000\000\000\000\
    \105\000\105\000\107\000\105\000\105\000\105\000\106\000\106\000\
    \106\000\106\000\106\000\106\000\106\000\106\000\106\000\106\000\
    \105\000\000\000\105\000\105\000\105\000\105\000\105\000\000\000\
    \000\000\107\000\003\000\000\000\000\000\003\000\003\000\003\000\
    \000\000\000\000\104\000\103\000\003\000\000\000\003\000\003\000\
    \003\000\106\000\106\000\106\000\106\000\106\000\106\000\106\000\
    \106\000\106\000\106\000\003\000\105\000\003\000\003\000\003\000\
    \003\000\003\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\000\000\000\000\
    \000\000\000\000\105\000\073\000\105\000\000\000\075\000\003\000\
    \075\000\075\000\075\000\075\000\075\000\075\000\075\000\075\000\
    \075\000\075\000\075\000\075\000\075\000\075\000\075\000\075\000\
    \075\000\075\000\075\000\075\000\075\000\075\000\075\000\075\000\
    \075\000\075\000\000\000\074\000\000\000\003\000\075\000\003\000\
    \075\000\075\000\075\000\075\000\075\000\075\000\075\000\075\000\
    \075\000\075\000\075\000\075\000\075\000\075\000\075\000\075\000\
    \075\000\075\000\075\000\075\000\075\000\075\000\075\000\075\000\
    \075\000\075\000\055\000\074\000\000\000\000\000\000\000\000\000\
    \000\000\057\000\000\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\055\000\055\000\055\000\055\000\
    \056\000\055\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\000\000\000\000\
    \000\000\000\000\030\000\000\000\055\000\055\000\055\000\055\000\
    \056\000\055\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\055\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\057\000\000\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \055\000\059\000\055\000\055\000\056\000\055\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\060\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\061\000\
    \058\000\058\000\000\000\000\000\000\000\000\000\030\000\000\000\
    \055\000\059\000\055\000\055\000\056\000\055\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\060\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\061\000\
    \058\000\058\000\032\000\195\000\195\000\195\000\195\000\195\000\
    \195\000\195\000\195\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\000\000\000\000\
    \000\000\000\000\032\000\000\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\096\000\096\000\
    \096\000\096\000\096\000\096\000\096\000\096\000\096\000\096\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\096\000\
    \096\000\096\000\096\000\096\000\096\000\191\000\191\000\191\000\
    \191\000\191\000\191\000\191\000\191\000\191\000\191\000\192\000\
    \192\000\192\000\192\000\192\000\192\000\192\000\192\000\192\000\
    \192\000\000\000\000\000\000\000\000\000\000\000\000\000\096\000\
    \096\000\096\000\096\000\096\000\096\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\000\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\033\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\000\000\000\000\
    \000\000\000\000\033\000\000\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\112\000\108\000\
    \000\000\000\000\109\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\112\000\000\000\111\000\
    \000\000\000\000\000\000\000\000\145\000\000\000\000\000\146\000\
    \000\000\000\000\000\000\000\000\000\000\106\000\106\000\106\000\
    \106\000\106\000\106\000\106\000\106\000\106\000\106\000\000\000\
    \000\000\000\000\000\000\000\000\150\000\000\000\000\000\000\000\
    \000\000\148\000\152\000\000\000\151\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\000\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\034\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\149\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\000\000\000\000\
    \000\000\000\000\034\000\000\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\156\000\000\000\
    \156\000\156\000\156\000\156\000\156\000\156\000\156\000\156\000\
    \156\000\156\000\156\000\156\000\156\000\156\000\156\000\156\000\
    \156\000\156\000\156\000\156\000\156\000\156\000\156\000\156\000\
    \156\000\156\000\000\000\155\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\156\000\255\255\156\000\156\000\
    \156\000\156\000\156\000\156\000\156\000\156\000\156\000\156\000\
    \156\000\156\000\156\000\156\000\156\000\156\000\156\000\156\000\
    \156\000\156\000\156\000\156\000\156\000\156\000\156\000\156\000\
    \000\000\155\000\147\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\000\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\035\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\000\000\000\000\
    \000\000\000\000\035\000\000\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\171\000\171\000\
    \171\000\171\000\171\000\171\000\171\000\171\000\171\000\171\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\171\000\
    \171\000\171\000\171\000\171\000\171\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\171\000\
    \171\000\171\000\171\000\171\000\171\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\000\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\000\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\046\000\000\000\000\000\046\000\046\000\
    \046\000\000\000\000\000\000\000\046\000\046\000\000\000\046\000\
    \046\000\046\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\046\000\000\000\046\000\046\000\
    \046\000\046\000\046\000\000\000\210\000\000\000\210\000\210\000\
    \210\000\210\000\210\000\210\000\210\000\210\000\210\000\210\000\
    \210\000\210\000\210\000\210\000\210\000\210\000\210\000\210\000\
    \210\000\210\000\210\000\210\000\210\000\210\000\210\000\210\000\
    \046\000\052\000\209\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\000\000\046\000\046\000\
    \046\000\000\000\046\000\046\000\046\000\000\000\000\000\000\000\
    \046\000\046\000\000\000\046\000\046\000\046\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \046\000\000\000\046\000\046\000\046\000\046\000\046\000\000\000\
    \210\000\000\000\210\000\210\000\210\000\210\000\210\000\210\000\
    \210\000\210\000\210\000\210\000\210\000\210\000\210\000\210\000\
    \210\000\210\000\210\000\210\000\210\000\210\000\210\000\210\000\
    \210\000\210\000\210\000\210\000\046\000\048\000\209\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\000\000\046\000\000\000\046\000\000\000\000\000\000\000\
    \000\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\000\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\172\000\172\000\172\000\172\000\172\000\172\000\
    \172\000\172\000\172\000\172\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\172\000\172\000\172\000\172\000\172\000\
    \172\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\172\000\172\000\172\000\172\000\172\000\
    \172\000\000\000\000\000\000\000\000\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\035\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\000\000\000\000\000\000\000\000\035\000\000\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\196\000\196\000\196\000\196\000\196\000\196\000\196\000\
    \196\000\196\000\196\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\196\000\196\000\196\000\196\000\196\000\196\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\196\000\196\000\196\000\196\000\196\000\196\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\000\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\000\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\046\000\000\000\
    \000\000\046\000\046\000\046\000\000\000\000\000\000\000\046\000\
    \046\000\000\000\046\000\046\000\046\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\046\000\
    \000\000\046\000\046\000\046\000\046\000\046\000\000\000\000\000\
    \000\000\000\000\047\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\050\000\000\000\000\000\
    \000\000\000\000\000\000\046\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\000\000\000\000\
    \000\000\046\000\047\000\046\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\197\000\197\000\
    \197\000\197\000\197\000\197\000\197\000\197\000\197\000\197\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\197\000\
    \197\000\197\000\197\000\197\000\197\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\197\000\
    \197\000\197\000\197\000\197\000\197\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\000\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\048\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\049\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\000\000\000\000\
    \000\000\000\000\048\000\000\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\199\000\199\000\
    \199\000\199\000\199\000\199\000\199\000\199\000\199\000\199\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\199\000\
    \199\000\199\000\199\000\199\000\199\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\199\000\
    \199\000\199\000\199\000\199\000\199\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\000\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\051\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\054\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\000\000\000\000\
    \000\000\000\000\051\000\000\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\000\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\052\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\053\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\000\000\000\000\
    \000\000\000\000\052\000\000\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\000\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\055\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\000\000\000\000\
    \000\000\000\000\055\000\000\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\000\000\
    \000\000\000\000\072\000\000\000\072\000\000\000\000\000\071\000\
    \071\000\071\000\071\000\071\000\071\000\071\000\071\000\071\000\
    \071\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\000\000\000\000\000\000\000\000\055\000\000\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\057\000\057\000\057\000\057\000\057\000\
    \057\000\057\000\057\000\057\000\057\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\055\000\055\000\055\000\055\000\
    \056\000\055\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\000\000\000\000\
    \000\000\000\000\057\000\000\000\055\000\055\000\055\000\055\000\
    \056\000\055\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\055\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\000\000\000\000\000\000\000\000\055\000\000\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\070\000\070\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\000\000\000\000\
    \000\000\000\000\055\000\000\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\055\000\
    \055\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\000\000\000\000\000\000\000\000\055\000\000\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\062\000\062\000\062\000\062\000\062\000\
    \062\000\062\000\062\000\062\000\062\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\062\000\062\000\062\000\062\000\
    \062\000\062\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\000\000\000\000\
    \000\000\000\000\055\000\000\000\062\000\062\000\062\000\062\000\
    \062\000\062\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\063\000\000\000\062\000\
    \062\000\062\000\062\000\062\000\062\000\062\000\062\000\062\000\
    \062\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \062\000\062\000\062\000\062\000\062\000\062\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\064\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\000\000\000\000\000\000\000\000\062\000\000\000\
    \062\000\062\000\062\000\062\000\062\000\062\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\064\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\055\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\063\000\063\000\063\000\063\000\063\000\
    \063\000\063\000\063\000\063\000\063\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\063\000\063\000\063\000\063\000\
    \063\000\063\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\068\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\000\000\000\000\
    \000\000\000\000\063\000\000\000\063\000\063\000\063\000\063\000\
    \063\000\063\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\068\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\055\000\000\000\
    \000\000\000\000\066\000\000\000\066\000\000\000\000\000\065\000\
    \065\000\065\000\065\000\065\000\065\000\065\000\065\000\065\000\
    \065\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\000\000\000\000\000\000\000\000\055\000\000\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\065\000\065\000\065\000\065\000\065\000\
    \065\000\065\000\065\000\065\000\065\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\000\000\000\000\
    \000\000\000\000\065\000\000\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\055\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\000\000\000\000\000\000\000\000\055\000\000\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\000\000\000\000\000\000\066\000\000\000\
    \066\000\000\000\000\000\065\000\065\000\065\000\065\000\065\000\
    \065\000\065\000\065\000\065\000\065\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\000\000\000\000\
    \000\000\000\000\055\000\000\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\055\000\
    \055\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\000\000\000\000\000\000\000\000\069\000\000\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\055\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\070\000\070\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\000\000\000\000\
    \000\000\000\000\070\000\000\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\055\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\071\000\
    \071\000\071\000\071\000\071\000\071\000\071\000\071\000\071\000\
    \071\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\000\000\000\000\000\000\000\000\071\000\000\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\086\000\103\000\086\000\000\000\103\000\103\000\
    \103\000\086\000\000\000\000\000\103\000\103\000\000\000\103\000\
    \103\000\103\000\085\000\085\000\085\000\085\000\085\000\085\000\
    \085\000\085\000\085\000\085\000\103\000\000\000\103\000\103\000\
    \103\000\103\000\103\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\105\000\000\000\105\000\105\000\105\000\105\000\
    \000\000\000\000\000\000\105\000\105\000\000\000\105\000\105\000\
    \105\000\000\000\000\000\000\000\000\000\000\000\086\000\000\000\
    \103\000\000\000\000\000\105\000\086\000\105\000\105\000\105\000\
    \105\000\105\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \086\000\084\000\000\000\000\000\086\000\000\000\086\000\000\000\
    \006\000\000\000\083\000\006\000\006\000\006\000\103\000\000\000\
    \103\000\006\000\006\000\000\000\006\000\006\000\006\000\105\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\006\000\000\000\006\000\006\000\006\000\006\000\006\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\117\000\
    \000\000\000\000\117\000\117\000\117\000\105\000\000\000\105\000\
    \117\000\117\000\000\000\117\000\117\000\117\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\006\000\000\000\000\000\
    \117\000\000\000\117\000\117\000\117\000\117\000\117\000\000\000\
    \000\000\000\000\117\000\000\000\000\000\117\000\117\000\117\000\
    \000\000\000\000\000\000\117\000\117\000\000\000\117\000\117\000\
    \117\000\000\000\000\000\006\000\000\000\006\000\000\000\000\000\
    \000\000\000\000\000\000\117\000\117\000\117\000\117\000\117\000\
    \117\000\117\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \117\000\000\000\000\000\117\000\117\000\117\000\000\000\000\000\
    \000\000\117\000\117\000\000\000\117\000\117\000\117\000\000\000\
    \000\000\000\000\117\000\000\000\117\000\000\000\000\000\117\000\
    \000\000\117\000\255\255\117\000\117\000\117\000\117\000\117\000\
    \000\000\000\000\000\000\120\000\000\000\000\000\120\000\120\000\
    \120\000\000\000\000\000\000\000\120\000\120\000\000\000\120\000\
    \120\000\120\000\000\000\000\000\000\000\117\000\000\000\117\000\
    \000\000\000\000\000\000\000\000\120\000\117\000\120\000\120\000\
    \120\000\120\000\120\000\000\000\000\000\000\000\006\000\000\000\
    \000\000\006\000\006\000\006\000\000\000\000\000\000\000\006\000\
    \006\000\000\000\006\000\006\000\006\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\117\000\000\000\117\000\000\000\006\000\
    \120\000\006\000\006\000\006\000\006\000\006\000\000\000\000\000\
    \000\000\006\000\000\000\000\000\006\000\006\000\006\000\000\000\
    \000\000\000\000\006\000\006\000\000\000\006\000\006\000\006\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\120\000\000\000\
    \120\000\000\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\000\000\000\000\000\000\139\000\000\000\000\000\139\000\
    \139\000\139\000\000\000\000\000\000\000\139\000\139\000\000\000\
    \139\000\139\000\139\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\006\000\000\000\006\000\000\000\139\000\006\000\139\000\
    \139\000\139\000\139\000\139\000\000\000\000\000\000\000\139\000\
    \000\000\000\000\139\000\139\000\139\000\000\000\000\000\000\000\
    \139\000\139\000\000\000\139\000\139\000\139\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\006\000\000\000\006\000\000\000\
    \139\000\139\000\139\000\139\000\139\000\139\000\139\000\000\000\
    \000\000\000\000\117\000\000\000\000\000\117\000\117\000\117\000\
    \000\000\000\000\000\000\117\000\117\000\000\000\117\000\117\000\
    \117\000\000\000\000\000\000\000\000\000\000\000\000\000\139\000\
    \000\000\139\000\000\000\117\000\139\000\117\000\117\000\117\000\
    \117\000\117\000\000\000\000\000\000\000\117\000\000\000\000\000\
    \117\000\117\000\117\000\000\000\000\000\000\000\117\000\117\000\
    \000\000\117\000\117\000\117\000\000\000\000\000\166\000\000\000\
    \166\000\000\000\139\000\000\000\139\000\166\000\117\000\117\000\
    \117\000\117\000\117\000\117\000\117\000\000\000\165\000\165\000\
    \165\000\165\000\165\000\165\000\165\000\165\000\165\000\165\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\117\000\000\000\117\000\
    \000\000\000\000\117\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\188\000\000\000\000\000\189\000\000\000\000\000\000\000\
    \000\000\000\000\166\000\000\000\000\000\000\000\000\000\000\000\
    \166\000\000\000\000\000\000\000\000\000\000\000\000\000\187\000\
    \117\000\187\000\117\000\000\000\166\000\000\000\187\000\000\000\
    \166\000\000\000\166\000\000\000\000\000\000\000\164\000\186\000\
    \186\000\186\000\186\000\186\000\186\000\186\000\186\000\186\000\
    \186\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\199\000\199\000\199\000\199\000\199\000\199\000\199\000\
    \199\000\199\000\199\000\187\000\000\000\000\000\000\000\000\000\
    \000\000\187\000\199\000\199\000\199\000\199\000\199\000\199\000\
    \000\000\000\000\000\000\000\000\000\000\187\000\185\000\000\000\
    \000\000\187\000\000\000\187\000\183\000\000\000\000\000\184\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\199\000\199\000\199\000\199\000\199\000\199\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\200\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\041\000\000\000\000\000\041\000\042\000\
    \044\000\045\000\042\000\044\000\045\000\079\000\109\000\115\000\
    \079\000\109\000\115\000\146\000\160\000\218\000\146\000\160\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\016\000\
    \013\000\017\000\026\000\039\000\017\000\017\000\039\000\066\000\
    \066\000\066\000\066\000\066\000\066\000\066\000\066\000\066\000\
    \066\000\077\000\078\000\084\000\084\000\084\000\084\000\013\000\
    \086\000\013\000\039\000\013\000\072\000\072\000\072\000\072\000\
    \072\000\072\000\072\000\072\000\072\000\072\000\085\000\085\000\
    \085\000\085\000\085\000\085\000\085\000\085\000\085\000\085\000\
    \088\000\088\000\088\000\088\000\088\000\088\000\088\000\088\000\
    \088\000\088\000\089\000\093\000\096\000\098\000\098\000\127\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\013\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\128\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\003\000\100\000\100\000\003\000\003\000\003\000\102\000\
    \102\000\027\000\003\000\003\000\027\000\003\000\003\000\003\000\
    \091\000\091\000\091\000\091\000\091\000\091\000\091\000\091\000\
    \110\000\132\000\003\000\110\000\003\000\003\000\003\000\003\000\
    \003\000\151\000\113\000\152\000\004\000\113\000\027\000\004\000\
    \004\000\004\000\158\000\159\000\166\000\004\000\004\000\169\000\
    \004\000\004\000\004\000\092\000\092\000\092\000\092\000\092\000\
    \092\000\092\000\092\000\172\000\183\000\004\000\003\000\004\000\
    \004\000\004\000\004\000\004\000\211\000\174\000\179\000\005\000\
    \174\000\179\000\005\000\005\000\005\000\212\000\215\000\217\000\
    \005\000\005\000\188\000\005\000\005\000\005\000\185\000\185\000\
    \185\000\185\000\111\000\027\000\003\000\111\000\003\000\255\255\
    \005\000\004\000\005\000\005\000\005\000\005\000\005\000\255\255\
    \189\000\188\000\006\000\189\000\255\255\006\000\006\000\006\000\
    \255\255\255\255\111\000\006\000\006\000\255\255\006\000\006\000\
    \006\000\255\255\255\255\190\000\112\000\112\000\190\000\004\000\
    \112\000\004\000\255\255\006\000\005\000\006\000\006\000\006\000\
    \006\000\006\000\255\255\201\000\202\000\007\000\201\000\202\000\
    \007\000\007\000\007\000\112\000\255\255\112\000\007\000\007\000\
    \255\255\007\000\007\000\007\000\207\000\208\000\213\000\207\000\
    \208\000\214\000\005\000\216\000\005\000\255\255\007\000\006\000\
    \007\000\007\000\007\000\007\000\007\000\255\255\255\255\255\255\
    \008\000\255\255\255\255\008\000\008\000\008\000\255\255\255\255\
    \148\000\008\000\008\000\148\000\008\000\008\000\008\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\006\000\255\255\006\000\
    \255\255\008\000\007\000\008\000\008\000\008\000\008\000\008\000\
    \255\255\255\255\255\255\010\000\255\255\148\000\010\000\010\000\
    \010\000\255\255\255\255\255\255\010\000\010\000\255\255\010\000\
    \010\000\010\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \007\000\255\255\007\000\255\255\010\000\008\000\010\000\010\000\
    \010\000\010\000\010\000\255\255\175\000\255\255\255\255\175\000\
    \011\000\255\255\255\255\011\000\011\000\011\000\202\000\027\000\
    \255\255\011\000\011\000\255\255\011\000\011\000\011\000\255\255\
    \255\255\255\255\148\000\008\000\175\000\008\000\110\000\010\000\
    \010\000\011\000\255\255\011\000\011\000\011\000\011\000\011\000\
    \113\000\255\255\255\255\255\255\255\255\014\000\255\255\255\255\
    \014\000\014\000\014\000\255\255\255\255\255\255\014\000\014\000\
    \255\255\014\000\014\000\014\000\255\255\255\255\010\000\010\000\
    \010\000\255\255\255\255\255\255\011\000\011\000\014\000\255\255\
    \014\000\014\000\014\000\014\000\014\000\255\255\255\255\255\255\
    \015\000\255\255\255\255\015\000\015\000\015\000\175\000\255\255\
    \255\255\015\000\015\000\255\255\015\000\015\000\015\000\255\255\
    \111\000\255\255\255\255\011\000\255\255\011\000\255\255\255\255\
    \255\255\015\000\014\000\015\000\015\000\015\000\015\000\015\000\
    \255\255\255\255\018\000\255\255\255\255\018\000\018\000\018\000\
    \255\255\255\255\255\255\018\000\018\000\255\255\018\000\018\000\
    \018\000\255\255\255\255\112\000\255\255\255\255\255\255\255\255\
    \014\000\255\255\014\000\018\000\255\255\015\000\018\000\018\000\
    \018\000\018\000\202\000\255\255\255\255\019\000\255\255\255\255\
    \019\000\019\000\019\000\255\255\255\255\255\255\019\000\019\000\
    \255\255\019\000\019\000\019\000\213\000\255\255\255\255\214\000\
    \255\255\216\000\255\255\015\000\255\255\015\000\019\000\018\000\
    \019\000\019\000\019\000\019\000\019\000\255\255\255\255\255\255\
    \023\000\255\255\255\255\023\000\023\000\023\000\148\000\255\255\
    \255\255\023\000\023\000\255\255\023\000\023\000\023\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\018\000\255\255\018\000\
    \255\255\023\000\019\000\023\000\023\000\023\000\023\000\023\000\
    \083\000\083\000\083\000\083\000\083\000\083\000\083\000\083\000\
    \083\000\083\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\083\000\083\000\083\000\083\000\083\000\083\000\255\255\
    \019\000\255\255\019\000\255\255\255\255\023\000\255\255\255\255\
    \255\255\255\255\175\000\255\255\255\255\255\255\255\255\024\000\
    \193\000\193\000\193\000\193\000\193\000\193\000\193\000\193\000\
    \255\255\083\000\083\000\083\000\083\000\083\000\083\000\255\255\
    \255\255\255\255\255\255\023\000\255\255\023\000\024\000\024\000\
    \255\255\024\000\024\000\024\000\024\000\255\255\255\255\255\255\
    \024\000\024\000\107\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\255\255\024\000\024\000\024\000\024\000\024\000\255\255\
    \255\255\107\000\025\000\255\255\255\255\025\000\025\000\025\000\
    \255\255\255\255\025\000\025\000\025\000\255\255\025\000\025\000\
    \025\000\107\000\107\000\107\000\107\000\107\000\107\000\107\000\
    \107\000\107\000\107\000\025\000\024\000\025\000\025\000\025\000\
    \025\000\025\000\165\000\165\000\165\000\165\000\165\000\165\000\
    \165\000\165\000\165\000\165\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\255\255\255\255\
    \255\255\255\255\024\000\028\000\024\000\255\255\075\000\025\000\
    \075\000\075\000\075\000\075\000\075\000\075\000\075\000\075\000\
    \075\000\075\000\075\000\075\000\075\000\075\000\075\000\075\000\
    \075\000\075\000\075\000\075\000\075\000\075\000\075\000\075\000\
    \075\000\075\000\255\255\075\000\255\255\025\000\028\000\025\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\030\000\028\000\255\255\255\255\255\255\255\255\
    \255\255\030\000\255\255\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\255\255\255\255\
    \255\255\255\255\030\000\255\255\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\031\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\031\000\255\255\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\255\255\255\255\255\255\255\255\031\000\255\255\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\032\000\194\000\194\000\194\000\194\000\194\000\
    \194\000\194\000\194\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\255\255\255\255\
    \255\255\255\255\032\000\255\255\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\095\000\095\000\
    \095\000\095\000\095\000\095\000\095\000\095\000\095\000\095\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\095\000\
    \095\000\095\000\095\000\095\000\095\000\186\000\186\000\186\000\
    \186\000\186\000\186\000\186\000\186\000\186\000\186\000\191\000\
    \191\000\191\000\191\000\191\000\191\000\191\000\191\000\191\000\
    \191\000\255\255\255\255\255\255\255\255\255\255\255\255\095\000\
    \095\000\095\000\095\000\095\000\095\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\255\255\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\033\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\255\255\255\255\
    \255\255\255\255\033\000\255\255\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\106\000\106\000\
    \255\255\255\255\106\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\106\000\255\255\106\000\
    \255\255\255\255\255\255\255\255\143\000\255\255\255\255\143\000\
    \255\255\255\255\255\255\255\255\255\255\106\000\106\000\106\000\
    \106\000\106\000\106\000\106\000\106\000\106\000\106\000\255\255\
    \255\255\255\255\255\255\255\255\143\000\255\255\255\255\255\255\
    \255\255\143\000\143\000\255\255\143\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\255\255\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\034\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\143\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\255\255\255\255\
    \255\255\255\255\034\000\255\255\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\149\000\255\255\
    \149\000\149\000\149\000\149\000\149\000\149\000\149\000\149\000\
    \149\000\149\000\149\000\149\000\149\000\149\000\149\000\149\000\
    \149\000\149\000\149\000\149\000\149\000\149\000\149\000\149\000\
    \149\000\149\000\255\255\149\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\156\000\106\000\156\000\156\000\
    \156\000\156\000\156\000\156\000\156\000\156\000\156\000\156\000\
    \156\000\156\000\156\000\156\000\156\000\156\000\156\000\156\000\
    \156\000\156\000\156\000\156\000\156\000\156\000\156\000\156\000\
    \255\255\156\000\143\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\255\255\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\035\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\255\255\255\255\
    \255\255\255\255\035\000\255\255\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\164\000\164\000\
    \164\000\164\000\164\000\164\000\164\000\164\000\164\000\164\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\164\000\
    \164\000\164\000\164\000\164\000\164\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\164\000\
    \164\000\164\000\164\000\164\000\164\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\255\255\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\255\255\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\036\000\255\255\255\255\036\000\036\000\
    \036\000\255\255\255\255\255\255\036\000\036\000\255\255\036\000\
    \036\000\036\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\036\000\255\255\036\000\036\000\
    \036\000\036\000\036\000\255\255\204\000\255\255\204\000\204\000\
    \204\000\204\000\204\000\204\000\204\000\204\000\204\000\204\000\
    \204\000\204\000\204\000\204\000\204\000\204\000\204\000\204\000\
    \204\000\204\000\204\000\204\000\204\000\204\000\204\000\204\000\
    \036\000\036\000\204\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\255\255\036\000\037\000\
    \036\000\255\255\037\000\037\000\037\000\255\255\255\255\255\255\
    \037\000\037\000\255\255\037\000\037\000\037\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \037\000\255\255\037\000\037\000\037\000\037\000\037\000\255\255\
    \210\000\255\255\210\000\210\000\210\000\210\000\210\000\210\000\
    \210\000\210\000\210\000\210\000\210\000\210\000\210\000\210\000\
    \210\000\210\000\210\000\210\000\210\000\210\000\210\000\210\000\
    \210\000\210\000\210\000\210\000\037\000\037\000\210\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\255\255\037\000\255\255\037\000\255\255\255\255\255\255\
    \255\255\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\255\255\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\171\000\171\000\171\000\171\000\171\000\171\000\
    \171\000\171\000\171\000\171\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\171\000\171\000\171\000\171\000\171\000\
    \171\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\171\000\171\000\171\000\171\000\171\000\
    \171\000\255\255\255\255\255\255\255\255\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\038\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\038\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\255\255\255\255\255\255\255\255\038\000\255\255\038\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\184\000\184\000\184\000\184\000\184\000\184\000\184\000\
    \184\000\184\000\184\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\184\000\184\000\184\000\184\000\184\000\184\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\184\000\184\000\184\000\184\000\184\000\184\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\255\255\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\255\255\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\046\000\255\255\
    \255\255\046\000\046\000\046\000\255\255\255\255\255\255\046\000\
    \046\000\255\255\046\000\046\000\046\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\046\000\
    \255\255\046\000\046\000\046\000\046\000\046\000\255\255\255\255\
    \255\255\255\255\047\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\255\255\255\255\
    \255\255\255\255\255\255\046\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\255\255\255\255\
    \255\255\046\000\047\000\046\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\196\000\196\000\
    \196\000\196\000\196\000\196\000\196\000\196\000\196\000\196\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\196\000\
    \196\000\196\000\196\000\196\000\196\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\196\000\
    \196\000\196\000\196\000\196\000\196\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\255\255\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\048\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\255\255\255\255\
    \255\255\255\255\048\000\255\255\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\198\000\198\000\
    \198\000\198\000\198\000\198\000\198\000\198\000\198\000\198\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\198\000\
    \198\000\198\000\198\000\198\000\198\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\198\000\
    \198\000\198\000\198\000\198\000\198\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\255\255\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\051\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\255\255\255\255\
    \255\255\255\255\051\000\255\255\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\255\255\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\052\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\255\255\255\255\
    \255\255\255\255\052\000\255\255\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\255\255\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\055\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\255\255\255\255\
    \255\255\255\255\055\000\255\255\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\056\000\255\255\
    \255\255\255\255\056\000\255\255\056\000\255\255\255\255\056\000\
    \056\000\056\000\056\000\056\000\056\000\056\000\056\000\056\000\
    \056\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \056\000\056\000\056\000\056\000\056\000\056\000\056\000\056\000\
    \056\000\056\000\056\000\056\000\056\000\056\000\056\000\056\000\
    \056\000\056\000\056\000\056\000\056\000\056\000\056\000\056\000\
    \056\000\056\000\255\255\255\255\255\255\255\255\056\000\255\255\
    \056\000\056\000\056\000\056\000\056\000\056\000\056\000\056\000\
    \056\000\056\000\056\000\056\000\056\000\056\000\056\000\056\000\
    \056\000\056\000\056\000\056\000\056\000\056\000\056\000\056\000\
    \056\000\056\000\057\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\057\000\057\000\057\000\057\000\057\000\
    \057\000\057\000\057\000\057\000\057\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\057\000\057\000\057\000\057\000\
    \057\000\057\000\057\000\057\000\057\000\057\000\057\000\057\000\
    \057\000\057\000\057\000\057\000\057\000\057\000\057\000\057\000\
    \057\000\057\000\057\000\057\000\057\000\057\000\255\255\255\255\
    \255\255\255\255\057\000\255\255\057\000\057\000\057\000\057\000\
    \057\000\057\000\057\000\057\000\057\000\057\000\057\000\057\000\
    \057\000\057\000\057\000\057\000\057\000\057\000\057\000\057\000\
    \057\000\057\000\057\000\057\000\057\000\057\000\058\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\255\255\255\255\255\255\255\255\058\000\255\255\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\059\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\059\000\059\000\059\000\059\000\059\000\
    \059\000\059\000\059\000\059\000\059\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\059\000\059\000\059\000\059\000\
    \059\000\059\000\059\000\059\000\059\000\059\000\059\000\059\000\
    \059\000\059\000\059\000\059\000\059\000\059\000\059\000\059\000\
    \059\000\059\000\059\000\059\000\059\000\059\000\255\255\255\255\
    \255\255\255\255\059\000\255\255\059\000\059\000\059\000\059\000\
    \059\000\059\000\059\000\059\000\059\000\059\000\059\000\059\000\
    \059\000\059\000\059\000\059\000\059\000\059\000\059\000\059\000\
    \059\000\059\000\059\000\059\000\059\000\059\000\060\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\060\000\
    \060\000\060\000\060\000\060\000\060\000\060\000\060\000\060\000\
    \060\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \060\000\060\000\060\000\060\000\060\000\060\000\060\000\060\000\
    \060\000\060\000\060\000\060\000\060\000\060\000\060\000\060\000\
    \060\000\060\000\060\000\060\000\060\000\060\000\060\000\060\000\
    \060\000\060\000\255\255\255\255\255\255\255\255\060\000\255\255\
    \060\000\060\000\060\000\060\000\060\000\060\000\060\000\060\000\
    \060\000\060\000\060\000\060\000\060\000\060\000\060\000\060\000\
    \060\000\060\000\060\000\060\000\060\000\060\000\060\000\060\000\
    \060\000\060\000\061\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\061\000\061\000\061\000\061\000\061\000\
    \061\000\061\000\061\000\061\000\061\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\061\000\061\000\061\000\061\000\
    \061\000\061\000\061\000\061\000\061\000\061\000\061\000\061\000\
    \061\000\061\000\061\000\061\000\061\000\061\000\061\000\061\000\
    \061\000\061\000\061\000\061\000\061\000\061\000\255\255\255\255\
    \255\255\255\255\061\000\255\255\061\000\061\000\061\000\061\000\
    \061\000\061\000\061\000\061\000\061\000\061\000\061\000\061\000\
    \061\000\061\000\061\000\061\000\061\000\061\000\061\000\061\000\
    \061\000\061\000\061\000\061\000\061\000\061\000\062\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\062\000\255\255\062\000\
    \062\000\062\000\062\000\062\000\062\000\062\000\062\000\062\000\
    \062\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \062\000\062\000\062\000\062\000\062\000\062\000\062\000\062\000\
    \062\000\062\000\062\000\062\000\062\000\062\000\062\000\062\000\
    \062\000\062\000\062\000\062\000\062\000\062\000\062\000\062\000\
    \062\000\062\000\255\255\255\255\255\255\255\255\062\000\255\255\
    \062\000\062\000\062\000\062\000\062\000\062\000\062\000\062\000\
    \062\000\062\000\062\000\062\000\062\000\062\000\062\000\062\000\
    \062\000\062\000\062\000\062\000\062\000\062\000\062\000\062\000\
    \062\000\062\000\063\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\063\000\063\000\063\000\063\000\063\000\
    \063\000\063\000\063\000\063\000\063\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\063\000\063\000\063\000\063\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\063\000\063\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\063\000\063\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\255\255\255\255\
    \255\255\255\255\063\000\255\255\063\000\063\000\063\000\063\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\063\000\063\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\063\000\063\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\064\000\255\255\
    \255\255\255\255\064\000\255\255\064\000\255\255\255\255\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \064\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \064\000\064\000\255\255\255\255\255\255\255\255\064\000\255\255\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \064\000\064\000\065\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\065\000\065\000\065\000\065\000\065\000\
    \065\000\065\000\065\000\065\000\065\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\065\000\065\000\065\000\065\000\
    \065\000\065\000\065\000\065\000\065\000\065\000\065\000\065\000\
    \065\000\065\000\065\000\065\000\065\000\065\000\065\000\065\000\
    \065\000\065\000\065\000\065\000\065\000\065\000\255\255\255\255\
    \255\255\255\255\065\000\255\255\065\000\065\000\065\000\065\000\
    \065\000\065\000\065\000\065\000\065\000\065\000\065\000\065\000\
    \065\000\065\000\065\000\065\000\065\000\065\000\065\000\065\000\
    \065\000\065\000\065\000\065\000\065\000\065\000\067\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\255\255\255\255\255\255\255\255\067\000\255\255\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\068\000\255\255\255\255\255\255\068\000\255\255\
    \068\000\255\255\255\255\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\255\255\255\255\
    \255\255\255\255\068\000\255\255\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\069\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\255\255\255\255\255\255\255\255\069\000\255\255\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\070\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\070\000\070\000\070\000\070\000\070\000\
    \070\000\070\000\070\000\070\000\070\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\070\000\070\000\070\000\070\000\
    \070\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
    \070\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
    \070\000\070\000\070\000\070\000\070\000\070\000\255\255\255\255\
    \255\255\255\255\070\000\255\255\070\000\070\000\070\000\070\000\
    \070\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
    \070\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
    \070\000\070\000\070\000\070\000\070\000\070\000\071\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\071\000\
    \071\000\071\000\071\000\071\000\071\000\071\000\071\000\071\000\
    \071\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \071\000\071\000\071\000\071\000\071\000\071\000\071\000\071\000\
    \071\000\071\000\071\000\071\000\071\000\071\000\071\000\071\000\
    \071\000\071\000\071\000\071\000\071\000\071\000\071\000\071\000\
    \071\000\071\000\255\255\255\255\255\255\255\255\071\000\255\255\
    \071\000\071\000\071\000\071\000\071\000\071\000\071\000\071\000\
    \071\000\071\000\071\000\071\000\071\000\071\000\071\000\071\000\
    \071\000\071\000\071\000\071\000\071\000\071\000\071\000\071\000\
    \071\000\071\000\076\000\103\000\076\000\255\255\103\000\103\000\
    \103\000\076\000\255\255\255\255\103\000\103\000\255\255\103\000\
    \103\000\103\000\076\000\076\000\076\000\076\000\076\000\076\000\
    \076\000\076\000\076\000\076\000\103\000\255\255\103\000\103\000\
    \103\000\103\000\103\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\105\000\255\255\105\000\105\000\105\000\105\000\
    \255\255\255\255\255\255\105\000\105\000\255\255\105\000\105\000\
    \105\000\255\255\255\255\255\255\255\255\255\255\076\000\255\255\
    \103\000\255\255\255\255\105\000\076\000\105\000\105\000\105\000\
    \105\000\105\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \076\000\076\000\255\255\255\255\076\000\255\255\076\000\255\255\
    \116\000\255\255\076\000\116\000\116\000\116\000\103\000\255\255\
    \103\000\116\000\116\000\255\255\116\000\116\000\116\000\105\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\116\000\255\255\116\000\116\000\116\000\116\000\116\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\117\000\
    \255\255\255\255\117\000\117\000\117\000\105\000\255\255\105\000\
    \117\000\117\000\255\255\117\000\117\000\117\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\116\000\255\255\255\255\
    \117\000\255\255\117\000\117\000\117\000\117\000\117\000\255\255\
    \255\255\255\255\118\000\255\255\255\255\118\000\118\000\118\000\
    \255\255\255\255\255\255\118\000\118\000\255\255\118\000\118\000\
    \118\000\255\255\255\255\116\000\255\255\116\000\255\255\255\255\
    \255\255\255\255\255\255\118\000\117\000\118\000\118\000\118\000\
    \118\000\118\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \119\000\255\255\255\255\119\000\119\000\119\000\255\255\255\255\
    \255\255\119\000\119\000\255\255\119\000\119\000\119\000\255\255\
    \255\255\255\255\117\000\255\255\117\000\255\255\255\255\118\000\
    \255\255\119\000\076\000\119\000\119\000\119\000\119\000\119\000\
    \255\255\255\255\255\255\120\000\255\255\255\255\120\000\120\000\
    \120\000\255\255\255\255\255\255\120\000\120\000\255\255\120\000\
    \120\000\120\000\255\255\255\255\255\255\118\000\255\255\118\000\
    \255\255\255\255\255\255\255\255\120\000\119\000\120\000\120\000\
    \120\000\120\000\120\000\255\255\255\255\255\255\126\000\255\255\
    \255\255\126\000\126\000\126\000\255\255\255\255\255\255\126\000\
    \126\000\255\255\126\000\126\000\126\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\119\000\255\255\119\000\255\255\126\000\
    \120\000\126\000\126\000\126\000\126\000\126\000\255\255\255\255\
    \255\255\136\000\255\255\255\255\136\000\136\000\136\000\255\255\
    \255\255\255\255\136\000\136\000\255\255\136\000\136\000\136\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\120\000\255\255\
    \120\000\255\255\136\000\126\000\136\000\136\000\136\000\136\000\
    \136\000\255\255\255\255\255\255\139\000\255\255\255\255\139\000\
    \139\000\139\000\255\255\255\255\255\255\139\000\139\000\255\255\
    \139\000\139\000\139\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\126\000\255\255\126\000\255\255\139\000\136\000\139\000\
    \139\000\139\000\139\000\139\000\255\255\255\255\255\255\140\000\
    \255\255\255\255\140\000\140\000\140\000\255\255\255\255\255\255\
    \140\000\140\000\255\255\140\000\140\000\140\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\136\000\255\255\136\000\255\255\
    \140\000\139\000\140\000\140\000\140\000\140\000\140\000\255\255\
    \255\255\255\255\141\000\255\255\255\255\141\000\141\000\141\000\
    \255\255\255\255\255\255\141\000\141\000\255\255\141\000\141\000\
    \141\000\255\255\255\255\255\255\255\255\255\255\255\255\139\000\
    \255\255\139\000\255\255\141\000\140\000\141\000\141\000\141\000\
    \141\000\141\000\255\255\255\255\255\255\142\000\255\255\255\255\
    \142\000\142\000\142\000\255\255\255\255\255\255\142\000\142\000\
    \255\255\142\000\142\000\142\000\255\255\255\255\157\000\255\255\
    \157\000\255\255\140\000\255\255\140\000\157\000\142\000\141\000\
    \142\000\142\000\142\000\142\000\142\000\255\255\157\000\157\000\
    \157\000\157\000\157\000\157\000\157\000\157\000\157\000\157\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\141\000\255\255\141\000\
    \255\255\255\255\142\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\180\000\255\255\255\255\180\000\255\255\255\255\255\255\
    \255\255\255\255\157\000\255\255\255\255\255\255\255\255\255\255\
    \157\000\255\255\255\255\255\255\255\255\255\255\255\255\180\000\
    \142\000\180\000\142\000\255\255\157\000\255\255\180\000\255\255\
    \157\000\255\255\157\000\255\255\255\255\255\255\157\000\180\000\
    \180\000\180\000\180\000\180\000\180\000\180\000\180\000\180\000\
    \180\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\199\000\199\000\199\000\199\000\199\000\199\000\199\000\
    \199\000\199\000\199\000\180\000\255\255\255\255\255\255\255\255\
    \255\255\180\000\199\000\199\000\199\000\199\000\199\000\199\000\
    \255\255\255\255\255\255\255\255\255\255\180\000\180\000\255\255\
    \255\255\180\000\255\255\180\000\180\000\255\255\255\255\180\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\199\000\199\000\199\000\199\000\199\000\199\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\199\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\180\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255";
  Lexing.lex_base_code =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\010\000\036\000\022\000\000\000\000\000\000\000\
    \005\000\000\000\039\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\001\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\002\000\005\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000";
  Lexing.lex_backtrk_code =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\053\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000";
  Lexing.lex_default_code =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\031\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000";
  Lexing.lex_trans_code =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\001\000\000\000\050\000\050\000\000\000\009\000\050\000\
    \000\000\000\000\000\000\009\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \001\000\000\000\009\000\001\000\000\000\009\000\000\000\034\000\
    \000\000\000\000\009\000\000\000\012\000\001\000\000\000\000\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\001\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000";
  Lexing.lex_check_code =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\024\000\111\000\180\000\189\000\111\000\112\000\190\000\
    \255\255\255\255\255\255\106\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \024\000\255\255\111\000\000\000\255\255\112\000\255\255\112\000\
    \255\255\255\255\106\000\255\255\106\000\107\000\255\255\255\255\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\106\000\106\000\106\000\106\000\106\000\106\000\
    \106\000\106\000\106\000\106\000\107\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\107\000\107\000\107\000\107\000\
    \107\000\107\000\107\000\107\000\107\000\107\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \111\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255";
  Lexing.lex_code =
   "\255\005\255\255\007\255\006\255\255\007\255\255\009\255\008\255\
    \255\006\255\007\255\255\004\255\000\005\001\006\002\007\255\009\
    \255\255\008\255\009\255\255\000\005\001\006\004\008\003\009\002\
    \007\255\001\255\255\000\001\255";
}

let rec token lexbuf =
  lexbuf.Lexing.lex_mem <- Array.make 10 (-1); __ocaml_lex_token_rec lexbuf 0
and __ocaml_lex_token_rec lexbuf __ocaml_lex_state =
  match Lexing.new_engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 770 "parsing/lexer.mll"
                 (
      if not !escaped_newlines then
        raise (Error(Illegal_character (Lexing.lexeme_char lexbuf 0),
                     Location.curr lexbuf));
      update_loc lexbuf None 1 false 0;
      token lexbuf )
# 2358 "parsing/lexer.ml"

  | 1 ->
# 777 "parsing/lexer.mll"
      ( update_loc lexbuf None 1 false 0;
        EOL )
# 2364 "parsing/lexer.ml"

  | 2 ->
# 780 "parsing/lexer.mll"
      ( token lexbuf )
# 2369 "parsing/lexer.ml"

  | 3 ->
# 782 "parsing/lexer.mll"
      ( UNDERSCORE )
# 2374 "parsing/lexer.ml"

  | 4 ->
# 784 "parsing/lexer.mll"
      ( TILDE )
# 2379 "parsing/lexer.ml"

  | 5 ->
# 786 "parsing/lexer.mll"
      ( LABEL (get_label_name lexbuf) )
# 2384 "parsing/lexer.ml"

  | 6 ->
# 788 "parsing/lexer.mll"
      ( warn_latin1 lexbuf; LABEL (get_label_name lexbuf) )
# 2389 "parsing/lexer.ml"

  | 7 ->
# 790 "parsing/lexer.mll"
      ( QUESTION )
# 2394 "parsing/lexer.ml"

  | 8 ->
# 792 "parsing/lexer.mll"
      ( OPTLABEL (get_label_name lexbuf) )
# 2399 "parsing/lexer.ml"

  | 9 ->
# 794 "parsing/lexer.mll"
      ( warn_latin1 lexbuf; OPTLABEL (get_label_name lexbuf) )
# 2404 "parsing/lexer.ml"

  | 10 ->
# 796 "parsing/lexer.mll"
      ( let s = Lexing.lexeme lexbuf in
        try Hashtbl.find keyword_table s
        with Not_found -> LIDENT s )
# 2411 "parsing/lexer.ml"

  | 11 ->
# 800 "parsing/lexer.mll"
      ( warn_latin1 lexbuf; LIDENT (Lexing.lexeme lexbuf) )
# 2416 "parsing/lexer.ml"

  | 12 ->
# 802 "parsing/lexer.mll"
      ( UIDENT(Lexing.lexeme lexbuf) )
# 2421 "parsing/lexer.ml"

  | 13 ->
# 804 "parsing/lexer.mll"
      ( warn_latin1 lexbuf; UIDENT(Lexing.lexeme lexbuf) )
# 2426 "parsing/lexer.ml"

  | 14 ->
# 805 "parsing/lexer.mll"
                ( INT (Lexing.lexeme lexbuf, None) )
# 2431 "parsing/lexer.ml"

  | 15 ->
let
# 806 "parsing/lexer.mll"
                    lit
# 2437 "parsing/lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos (lexbuf.Lexing.lex_curr_pos + -1)
and
# 806 "parsing/lexer.mll"
                                              modif
# 2442 "parsing/lexer.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_curr_pos + -1) in
# 807 "parsing/lexer.mll"
      ( INT (lit, Some modif) )
# 2446 "parsing/lexer.ml"

  | 16 ->
# 809 "parsing/lexer.mll"
      ( FLOAT (Lexing.lexeme lexbuf, None) )
# 2451 "parsing/lexer.ml"

  | 17 ->
let
# 810 "parsing/lexer.mll"
                                            lit
# 2457 "parsing/lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos (lexbuf.Lexing.lex_curr_pos + -1)
and
# 810 "parsing/lexer.mll"
                                                                      modif
# 2462 "parsing/lexer.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_curr_pos + -1) in
# 811 "parsing/lexer.mll"
      ( FLOAT (lit, Some modif) )
# 2466 "parsing/lexer.ml"

  | 18 ->
# 813 "parsing/lexer.mll"
      ( raise (Error(Invalid_literal (Lexing.lexeme lexbuf),
                     Location.curr lexbuf)) )
# 2472 "parsing/lexer.ml"

  | 19 ->
# 816 "parsing/lexer.mll"
      ( reset_string_buffer();
        is_in_string := true;
        let string_start = lexbuf.lex_start_p in
        string_start_loc := Location.curr lexbuf;
        string lexbuf;
        is_in_string := false;
        lexbuf.lex_start_p <- string_start;
        STRING (get_stored_string(), None) )
# 2484 "parsing/lexer.ml"

  | 20 ->
# 825 "parsing/lexer.mll"
      ( reset_string_buffer();
        let delim = Lexing.lexeme lexbuf in
        let delim = String.sub delim 1 (String.length delim - 2) in
        is_in_string := true;
        let string_start = lexbuf.lex_start_p in
        string_start_loc := Location.curr lexbuf;
        quoted_string delim lexbuf;
        is_in_string := false;
        lexbuf.lex_start_p <- string_start;
        STRING (get_stored_string(), Some delim) )
# 2498 "parsing/lexer.ml"

  | 21 ->
# 836 "parsing/lexer.mll"
      ( update_loc lexbuf None 1 false 1;
        CHAR (Lexing.lexeme_char lexbuf 1) )
# 2504 "parsing/lexer.ml"

  | 22 ->
# 839 "parsing/lexer.mll"
      ( CHAR(Lexing.lexeme_char lexbuf 1) )
# 2509 "parsing/lexer.ml"

  | 23 ->
# 841 "parsing/lexer.mll"
      ( CHAR(char_for_backslash (Lexing.lexeme_char lexbuf 2)) )
# 2514 "parsing/lexer.ml"

  | 24 ->
# 843 "parsing/lexer.mll"
      ( CHAR(char_for_decimal_code lexbuf 2) )
# 2519 "parsing/lexer.ml"

  | 25 ->
# 845 "parsing/lexer.mll"
      ( CHAR(char_for_octal_code lexbuf 3) )
# 2524 "parsing/lexer.ml"

  | 26 ->
# 847 "parsing/lexer.mll"
      ( CHAR(char_for_hexadecimal_code lexbuf 3) )
# 2529 "parsing/lexer.ml"

  | 27 ->
# 849 "parsing/lexer.mll"
      ( let l = Lexing.lexeme lexbuf in
        let esc = String.sub l 1 (String.length l - 1) in
        raise (Error(Illegal_escape esc, Location.curr lexbuf))
      )
# 2537 "parsing/lexer.ml"

  | 28 ->
# 854 "parsing/lexer.mll"
      ( let s, loc = with_comment_buffer comment lexbuf in
        COMMENT (s, loc) )
# 2543 "parsing/lexer.ml"

  | 29 ->
# 857 "parsing/lexer.mll"
      ( let s, loc = with_comment_buffer comment lexbuf in
        if !handle_docstrings then
          DOCSTRING (Docstrings.docstring s loc)
        else
          COMMENT ("*" ^ s, loc)
      )
# 2553 "parsing/lexer.ml"

  | 30 ->
let
# 863 "parsing/lexer.mll"
                     stars
# 2559 "parsing/lexer.ml"
= Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 3) lexbuf.Lexing.lex_curr_pos in
# 864 "parsing/lexer.mll"
      ( let s, loc =
          with_comment_buffer
            (fun lexbuf ->
               store_string ("*" ^ stars);
               comment lexbuf)
            lexbuf
        in
        COMMENT (s, loc) )
# 2570 "parsing/lexer.ml"

  | 31 ->
# 873 "parsing/lexer.mll"
      ( if !print_warnings then
          Location.prerr_warning (Location.curr lexbuf) Warnings.Comment_start;
        let s, loc = with_comment_buffer comment lexbuf in
        COMMENT (s, loc) )
# 2578 "parsing/lexer.ml"

  | 32 ->
let
# 877 "parsing/lexer.mll"
                    stars
# 2584 "parsing/lexer.ml"
= Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 2) (lexbuf.Lexing.lex_curr_pos + -2) in
# 878 "parsing/lexer.mll"
      ( if !handle_docstrings && stars="" then
         (* (**) is an empty docstring *)
          DOCSTRING(Docstrings.docstring "" (Location.curr lexbuf))
        else
          COMMENT (stars, Location.curr lexbuf) )
# 2592 "parsing/lexer.ml"

  | 33 ->
# 884 "parsing/lexer.mll"
      ( let loc = Location.curr lexbuf in
        Location.prerr_warning loc Warnings.Comment_not_end;
        lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_curr_pos - 1;
        let curpos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <- { curpos with pos_cnum = curpos.pos_cnum - 1 };
        STAR
      )
# 2603 "parsing/lexer.ml"

  | 34 ->
let
# 891 "parsing/lexer.mll"
                                    num
# 2609 "parsing/lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(0) lexbuf.Lexing.lex_mem.(1)
and
# 892 "parsing/lexer.mll"
                                            name
# 2614 "parsing/lexer.ml"
= Lexing.sub_lexeme_opt lexbuf lexbuf.Lexing.lex_mem.(4) lexbuf.Lexing.lex_mem.(3)
and
# 892 "parsing/lexer.mll"
                                                             directive
# 2619 "parsing/lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_mem.(2) in
# 894 "parsing/lexer.mll"
      (
        match int_of_string num with
        | exception _ ->
            (* PR#7165 *)
            let loc = Location.curr lexbuf in
            let explanation = "line number out of range" in
            let error = Invalid_directive (directive, Some explanation) in
            raise (Error (error, loc))
        | line_num ->
           (* Documentation says that the line number should be
              positive, but we have never guarded against this and it
              might have useful hackish uses. *)
            update_loc lexbuf name line_num true 0;
            token lexbuf
      )
# 2637 "parsing/lexer.ml"

  | 35 ->
# 909 "parsing/lexer.mll"
         ( HASH )
# 2642 "parsing/lexer.ml"

  | 36 ->
# 910 "parsing/lexer.mll"
         ( AMPERSAND )
# 2647 "parsing/lexer.ml"

  | 37 ->
# 911 "parsing/lexer.mll"
         ( AMPERAMPER )
# 2652 "parsing/lexer.ml"

  | 38 ->
# 912 "parsing/lexer.mll"
         ( BACKQUOTE )
# 2657 "parsing/lexer.ml"

  | 39 ->
# 913 "parsing/lexer.mll"
         ( QUOTE )
# 2662 "parsing/lexer.ml"

  | 40 ->
# 914 "parsing/lexer.mll"
         ( LPAREN )
# 2667 "parsing/lexer.ml"

  | 41 ->
# 915 "parsing/lexer.mll"
         ( RPAREN )
# 2672 "parsing/lexer.ml"

  | 42 ->
# 916 "parsing/lexer.mll"
         ( STAR )
# 2677 "parsing/lexer.ml"

  | 43 ->
# 917 "parsing/lexer.mll"
         ( COMMA )
# 2682 "parsing/lexer.ml"

  | 44 ->
# 918 "parsing/lexer.mll"
         ( MINUSGREATER )
# 2687 "parsing/lexer.ml"

  | 45 ->
# 919 "parsing/lexer.mll"
         ( DOT )
# 2692 "parsing/lexer.ml"

  | 46 ->
# 920 "parsing/lexer.mll"
         ( DOTDOT )
# 2697 "parsing/lexer.ml"

  | 47 ->
let
# 921 "parsing/lexer.mll"
                                      s
# 2703 "parsing/lexer.ml"
= Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 1) lexbuf.Lexing.lex_curr_pos in
# 921 "parsing/lexer.mll"
                                         ( DOTOP s )
# 2707 "parsing/lexer.ml"

  | 48 ->
# 922 "parsing/lexer.mll"
         ( COLON )
# 2712 "parsing/lexer.ml"

  | 49 ->
# 923 "parsing/lexer.mll"
         ( COLONCOLON )
# 2717 "parsing/lexer.ml"

  | 50 ->
# 924 "parsing/lexer.mll"
         ( COLONEQUAL )
# 2722 "parsing/lexer.ml"

  | 51 ->
# 925 "parsing/lexer.mll"
         ( COLONGREATER )
# 2727 "parsing/lexer.ml"

  | 52 ->
# 926 "parsing/lexer.mll"
         ( SEMI )
# 2732 "parsing/lexer.ml"

  | 53 ->
# 927 "parsing/lexer.mll"
         ( SEMISEMI )
# 2737 "parsing/lexer.ml"

  | 54 ->
# 928 "parsing/lexer.mll"
         ( LESS )
# 2742 "parsing/lexer.ml"

  | 55 ->
# 929 "parsing/lexer.mll"
         ( LESSMINUS )
# 2747 "parsing/lexer.ml"

  | 56 ->
# 930 "parsing/lexer.mll"
         ( EQUAL )
# 2752 "parsing/lexer.ml"

  | 57 ->
# 931 "parsing/lexer.mll"
         ( LBRACKET )
# 2757 "parsing/lexer.ml"

  | 58 ->
# 932 "parsing/lexer.mll"
         ( LBRACKETBAR )
# 2762 "parsing/lexer.ml"

  | 59 ->
# 933 "parsing/lexer.mll"
         ( LBRACKETLESS )
# 2767 "parsing/lexer.ml"

  | 60 ->
# 934 "parsing/lexer.mll"
         ( LBRACKETGREATER )
# 2772 "parsing/lexer.ml"

  | 61 ->
# 935 "parsing/lexer.mll"
         ( RBRACKET )
# 2777 "parsing/lexer.ml"

  | 62 ->
# 936 "parsing/lexer.mll"
         ( LBRACE )
# 2782 "parsing/lexer.ml"

  | 63 ->
# 937 "parsing/lexer.mll"
         ( LBRACELESS )
# 2787 "parsing/lexer.ml"

  | 64 ->
# 938 "parsing/lexer.mll"
         ( BAR )
# 2792 "parsing/lexer.ml"

  | 65 ->
# 939 "parsing/lexer.mll"
         ( BARBAR )
# 2797 "parsing/lexer.ml"

  | 66 ->
# 940 "parsing/lexer.mll"
         ( BARRBRACKET )
# 2802 "parsing/lexer.ml"

  | 67 ->
# 941 "parsing/lexer.mll"
         ( GREATER )
# 2807 "parsing/lexer.ml"

  | 68 ->
# 942 "parsing/lexer.mll"
         ( GREATERRBRACKET )
# 2812 "parsing/lexer.ml"

  | 69 ->
# 943 "parsing/lexer.mll"
         ( RBRACE )
# 2817 "parsing/lexer.ml"

  | 70 ->
# 944 "parsing/lexer.mll"
         ( GREATERRBRACE )
# 2822 "parsing/lexer.ml"

  | 71 ->
# 945 "parsing/lexer.mll"
         ( LBRACKETAT )
# 2827 "parsing/lexer.ml"

  | 72 ->
# 946 "parsing/lexer.mll"
           ( LBRACKETATAT )
# 2832 "parsing/lexer.ml"

  | 73 ->
# 947 "parsing/lexer.mll"
           ( LBRACKETATATAT )
# 2837 "parsing/lexer.ml"

  | 74 ->
# 948 "parsing/lexer.mll"
           ( LBRACKETPERCENT )
# 2842 "parsing/lexer.ml"

  | 75 ->
# 949 "parsing/lexer.mll"
           ( LBRACKETPERCENTPERCENT )
# 2847 "parsing/lexer.ml"

  | 76 ->
# 950 "parsing/lexer.mll"
         ( BANG )
# 2852 "parsing/lexer.ml"

  | 77 ->
# 951 "parsing/lexer.mll"
         ( INFIXOP0 "!=" )
# 2857 "parsing/lexer.ml"

  | 78 ->
# 952 "parsing/lexer.mll"
         ( PLUS )
# 2862 "parsing/lexer.ml"

  | 79 ->
# 953 "parsing/lexer.mll"
         ( PLUSDOT )
# 2867 "parsing/lexer.ml"

  | 80 ->
# 954 "parsing/lexer.mll"
         ( PLUSEQ )
# 2872 "parsing/lexer.ml"

  | 81 ->
# 955 "parsing/lexer.mll"
         ( MINUS )
# 2877 "parsing/lexer.ml"

  | 82 ->
# 956 "parsing/lexer.mll"
         ( MINUSDOT )
# 2882 "parsing/lexer.ml"

  | 83 ->
# 959 "parsing/lexer.mll"
            ( PREFIXOP(Lexing.lexeme lexbuf) )
# 2887 "parsing/lexer.ml"

  | 84 ->
# 961 "parsing/lexer.mll"
            ( PREFIXOP(Lexing.lexeme lexbuf) )
# 2892 "parsing/lexer.ml"

  | 85 ->
# 963 "parsing/lexer.mll"
            ( INFIXOP0(Lexing.lexeme lexbuf) )
# 2897 "parsing/lexer.ml"

  | 86 ->
# 965 "parsing/lexer.mll"
            ( INFIXOP1(Lexing.lexeme lexbuf) )
# 2902 "parsing/lexer.ml"

  | 87 ->
# 967 "parsing/lexer.mll"
            ( INFIXOP2(Lexing.lexeme lexbuf) )
# 2907 "parsing/lexer.ml"

  | 88 ->
# 969 "parsing/lexer.mll"
            ( INFIXOP4(Lexing.lexeme lexbuf) )
# 2912 "parsing/lexer.ml"

  | 89 ->
# 970 "parsing/lexer.mll"
            ( PERCENT )
# 2917 "parsing/lexer.ml"

  | 90 ->
# 972 "parsing/lexer.mll"
            ( INFIXOP3(Lexing.lexeme lexbuf) )
# 2922 "parsing/lexer.ml"

  | 91 ->
# 974 "parsing/lexer.mll"
            ( HASHOP(Lexing.lexeme lexbuf) )
# 2927 "parsing/lexer.ml"

  | 92 ->
# 975 "parsing/lexer.mll"
        (
    if !if_then_else <> Dir_out then
      if !if_then_else = Dir_if_true then
        raise (Error (Unterminated_if, Location.curr lexbuf))
      else raise (Error(Unterminated_else, Location.curr lexbuf))
    else 
      EOF

  )
# 2940 "parsing/lexer.ml"

  | 93 ->
# 985 "parsing/lexer.mll"
      ( raise (Error(Illegal_character (Lexing.lexeme_char lexbuf 0),
                     Location.curr lexbuf))
      )
# 2947 "parsing/lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_token_rec lexbuf __ocaml_lex_state

and comment lexbuf =
   __ocaml_lex_comment_rec lexbuf 143
and __ocaml_lex_comment_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 991 "parsing/lexer.mll"
      ( comment_start_loc := (Location.curr lexbuf) :: !comment_start_loc;
        store_lexeme lexbuf;
        comment lexbuf
      )
# 2962 "parsing/lexer.ml"

  | 1 ->
# 996 "parsing/lexer.mll"
      ( match !comment_start_loc with
        | [] -> assert false
        | [_] -> comment_start_loc := []; Location.curr lexbuf
        | _ :: l -> comment_start_loc := l;
                  store_lexeme lexbuf;
                  comment lexbuf
       )
# 2973 "parsing/lexer.ml"

  | 2 ->
# 1004 "parsing/lexer.mll"
      (
        string_start_loc := Location.curr lexbuf;
        store_string_char '\"';
        is_in_string := true;
        begin try string lexbuf
        with Error (Unterminated_string, str_start) ->
          match !comment_start_loc with
          | [] -> assert false
          | loc :: _ ->
            let start = List.hd (List.rev !comment_start_loc) in
            comment_start_loc := [];
            raise (Error (Unterminated_string_in_comment (start, str_start),
                          loc))
        end;
        is_in_string := false;
        store_string_char '\"';
        comment lexbuf )
# 2994 "parsing/lexer.ml"

  | 3 ->
# 1022 "parsing/lexer.mll"
      (
        let delim = Lexing.lexeme lexbuf in
        let delim = String.sub delim 1 (String.length delim - 2) in
        string_start_loc := Location.curr lexbuf;
        store_lexeme lexbuf;
        is_in_string := true;
        begin try quoted_string delim lexbuf
        with Error (Unterminated_string, str_start) ->
          match !comment_start_loc with
          | [] -> assert false
          | loc :: _ ->
            let start = List.hd (List.rev !comment_start_loc) in
            comment_start_loc := [];
            raise (Error (Unterminated_string_in_comment (start, str_start),
                          loc))
        end;
        is_in_string := false;
        store_string_char '|';
        store_string delim;
        store_string_char '}';
        comment lexbuf )
# 3019 "parsing/lexer.ml"

  | 4 ->
# 1045 "parsing/lexer.mll"
      ( store_lexeme lexbuf; comment lexbuf )
# 3024 "parsing/lexer.ml"

  | 5 ->
# 1047 "parsing/lexer.mll"
      ( update_loc lexbuf None 1 false 1;
        store_lexeme lexbuf;
        comment lexbuf
      )
# 3032 "parsing/lexer.ml"

  | 6 ->
# 1052 "parsing/lexer.mll"
      ( store_lexeme lexbuf; comment lexbuf )
# 3037 "parsing/lexer.ml"

  | 7 ->
# 1054 "parsing/lexer.mll"
      ( store_lexeme lexbuf; comment lexbuf )
# 3042 "parsing/lexer.ml"

  | 8 ->
# 1056 "parsing/lexer.mll"
      ( store_lexeme lexbuf; comment lexbuf )
# 3047 "parsing/lexer.ml"

  | 9 ->
# 1058 "parsing/lexer.mll"
      ( store_lexeme lexbuf; comment lexbuf )
# 3052 "parsing/lexer.ml"

  | 10 ->
# 1060 "parsing/lexer.mll"
      ( match !comment_start_loc with
        | [] -> assert false
        | loc :: _ ->
          let start = List.hd (List.rev !comment_start_loc) in
          comment_start_loc := [];
          raise (Error (Unterminated_comment start, loc))
      )
# 3063 "parsing/lexer.ml"

  | 11 ->
# 1068 "parsing/lexer.mll"
      ( update_loc lexbuf None 1 false 0;
        store_lexeme lexbuf;
        comment lexbuf
      )
# 3071 "parsing/lexer.ml"

  | 12 ->
# 1073 "parsing/lexer.mll"
      ( store_lexeme lexbuf; comment lexbuf )
# 3076 "parsing/lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_comment_rec lexbuf __ocaml_lex_state

and string lexbuf =
  lexbuf.Lexing.lex_mem <- Array.make 2 (-1); __ocaml_lex_string_rec lexbuf 175
and __ocaml_lex_string_rec lexbuf __ocaml_lex_state =
  match Lexing.new_engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 1077 "parsing/lexer.mll"
      ( () )
# 3088 "parsing/lexer.ml"

  | 1 ->
let
# 1078 "parsing/lexer.mll"
                                  space
# 3094 "parsing/lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(0) lexbuf.Lexing.lex_curr_pos in
# 1079 "parsing/lexer.mll"
      ( update_loc lexbuf None 1 false (String.length space);
        if in_comment () then store_lexeme lexbuf;
        string lexbuf
      )
# 3101 "parsing/lexer.ml"

  | 2 ->
# 1084 "parsing/lexer.mll"
      ( store_escaped_char lexbuf
                           (char_for_backslash(Lexing.lexeme_char lexbuf 1));
        string lexbuf )
# 3108 "parsing/lexer.ml"

  | 3 ->
# 1088 "parsing/lexer.mll"
      ( store_escaped_char lexbuf (char_for_decimal_code lexbuf 1);
         string lexbuf )
# 3114 "parsing/lexer.ml"

  | 4 ->
# 1091 "parsing/lexer.mll"
      ( store_escaped_char lexbuf (char_for_octal_code lexbuf 2);
         string lexbuf )
# 3120 "parsing/lexer.ml"

  | 5 ->
# 1094 "parsing/lexer.mll"
      ( store_escaped_char lexbuf (char_for_hexadecimal_code lexbuf 2);
         string lexbuf )
# 3126 "parsing/lexer.ml"

  | 6 ->
# 1097 "parsing/lexer.mll"
        ( store_escaped_uchar lexbuf (uchar_for_uchar_escape lexbuf);
          string lexbuf )
# 3132 "parsing/lexer.ml"

  | 7 ->
# 1100 "parsing/lexer.mll"
      ( if not (in_comment ()) then begin
(*  Should be an error, but we are very lax.
          raise (Error (Illegal_escape (Lexing.lexeme lexbuf),
                        Location.curr lexbuf))
*)
          let loc = Location.curr lexbuf in
          Location.prerr_warning loc Warnings.Illegal_backslash;
        end;
        store_lexeme lexbuf;
        string lexbuf
      )
# 3147 "parsing/lexer.ml"

  | 8 ->
# 1112 "parsing/lexer.mll"
      ( if not (in_comment ()) then
          Location.prerr_warning (Location.curr lexbuf) Warnings.Eol_in_string;
        update_loc lexbuf None 1 false 0;
        store_lexeme lexbuf;
        string lexbuf
      )
# 3157 "parsing/lexer.ml"

  | 9 ->
# 1119 "parsing/lexer.mll"
      ( is_in_string := false;
        raise (Error (Unterminated_string, !string_start_loc)) )
# 3163 "parsing/lexer.ml"

  | 10 ->
# 1122 "parsing/lexer.mll"
      ( store_string_char(Lexing.lexeme_char lexbuf 0);
        string lexbuf )
# 3169 "parsing/lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_string_rec lexbuf __ocaml_lex_state

and quoted_string delim lexbuf =
   __ocaml_lex_quoted_string_rec delim lexbuf 202
and __ocaml_lex_quoted_string_rec delim lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 1127 "parsing/lexer.mll"
      ( update_loc lexbuf None 1 false 0;
        store_lexeme lexbuf;
        quoted_string delim lexbuf
      )
# 3184 "parsing/lexer.ml"

  | 1 ->
# 1132 "parsing/lexer.mll"
      ( is_in_string := false;
        raise (Error (Unterminated_string, !string_start_loc)) )
# 3190 "parsing/lexer.ml"

  | 2 ->
# 1135 "parsing/lexer.mll"
      (
        let edelim = Lexing.lexeme lexbuf in
        let edelim = String.sub edelim 1 (String.length edelim - 2) in
        if delim = edelim then ()
        else (store_lexeme lexbuf; quoted_string delim lexbuf)
      )
# 3200 "parsing/lexer.ml"

  | 3 ->
# 1142 "parsing/lexer.mll"
      ( store_string_char(Lexing.lexeme_char lexbuf 0);
        quoted_string delim lexbuf )
# 3206 "parsing/lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_quoted_string_rec delim lexbuf __ocaml_lex_state

and skip_hash_bang lexbuf =
   __ocaml_lex_skip_hash_bang_rec lexbuf 211
and __ocaml_lex_skip_hash_bang_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 1147 "parsing/lexer.mll"
       ( update_loc lexbuf None 3 false 0 )
# 3218 "parsing/lexer.ml"

  | 1 ->
# 1149 "parsing/lexer.mll"
       ( update_loc lexbuf None 1 false 0 )
# 3223 "parsing/lexer.ml"

  | 2 ->
# 1150 "parsing/lexer.mll"
       ( () )
# 3228 "parsing/lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_skip_hash_bang_rec lexbuf __ocaml_lex_state

;;

# 1152 "parsing/lexer.mll"
 
  let at_bol lexbuf = 
    let pos = Lexing.lexeme_start_p lexbuf in 
    pos.pos_cnum = pos.pos_bol 

  let token_with_comments lexbuf =
    match !preprocessor with
    | None -> token lexbuf
    | Some (_init, preprocess) -> preprocess token lexbuf

  type newline_state =
    | NoLine (* There have been no blank lines yet. *)
    | NewLine
        (* There have been no blank lines, and the previous
           token was a newline. *)
    | BlankLine (* There have been blank lines. *)

  type doc_state =
    | Initial  (* There have been no docstrings yet *)
    | After of docstring list
        (* There have been docstrings, none of which were
           preceded by a blank line *)
    | Before of docstring list * docstring list * docstring list
        (* There have been docstrings, some of which were
           preceded by a blank line *)

  and docstring = Docstrings.docstring

  let interpret_directive lexbuf cont look_ahead = 
    let if_then_else = !if_then_else in
    begin match token_with_comments lexbuf, if_then_else with 
    |  IF, Dir_out  ->
        let rec skip_from_if_false () = 
          let token = token_with_comments lexbuf in
          if token = EOF then 
            raise (Error (Unterminated_if, Location.curr lexbuf)) else
          if token = HASH && at_bol lexbuf then 
            begin 
              let token = token_with_comments lexbuf in
              match token with
              | END -> 
                  begin
                    update_if_then_else Dir_out;
                    cont lexbuf
                  end
              | ELSE -> 
                  begin
                    update_if_then_else Dir_if_false;
                    cont lexbuf
                  end
              | IF ->
                  raise (Error (Unexpected_directive, Location.curr lexbuf))
              | _ -> 
                  if is_elif token &&
                     directive_parse token_with_comments lexbuf then
                    begin
                      update_if_then_else Dir_if_true;
                      cont lexbuf
                    end
                  else skip_from_if_false ()                               
            end
          else skip_from_if_false () in 
        if directive_parse token_with_comments lexbuf then
          begin 
            update_if_then_else Dir_if_true (* Next state: ELSE *);
            cont lexbuf
          end
        else
          skip_from_if_false ()
    | IF,  (Dir_if_false | Dir_if_true)->
        raise (Error(Unexpected_directive, Location.curr lexbuf))
    | LIDENT "elif", (Dir_if_false | Dir_out)
      -> (* when the predicate is false, it will continue eating `elif` *)
        raise (Error(Unexpected_directive, Location.curr lexbuf))
    | (LIDENT "elif" | ELSE as token), Dir_if_true ->           
        (* looking for #end, however, it can not see #if anymore *)
        let rec skip_from_if_true else_seen = 
          let token = token_with_comments lexbuf in
          if token = EOF then 
            raise (Error (Unterminated_else, Location.curr lexbuf)) else
          if token = HASH && at_bol lexbuf then 
            begin 
              let token = token_with_comments lexbuf in 
              match token with  
              | END -> 
                  begin
                    update_if_then_else Dir_out;
                    cont lexbuf
                  end  
              | IF ->  
                  raise (Error (Unexpected_directive, Location.curr lexbuf)) 
              | ELSE ->
                  if else_seen then 
                    raise (Error (Unexpected_directive, Location.curr lexbuf))
                  else 
                    skip_from_if_true true
              | _ ->
                  if else_seen && is_elif token then  
                    raise (Error (Unexpected_directive, Location.curr lexbuf))
                  else 
                    skip_from_if_true else_seen
            end
          else skip_from_if_true else_seen in 
        skip_from_if_true (token = ELSE)
    | ELSE, Dir_if_false 
    | ELSE, Dir_out -> 
        raise (Error(Unexpected_directive, Location.curr lexbuf))
    | END, (Dir_if_false | Dir_if_true ) -> 
        update_if_then_else  Dir_out;
        cont lexbuf
    | END,  Dir_out  -> 
        raise (Error(Unexpected_directive, Location.curr lexbuf))
    | token, (Dir_if_true | Dir_if_false | Dir_out) ->
        look_ahead token 
    end

  let token lexbuf =
    let post_pos = lexeme_end_p lexbuf in
    let attach lines docs pre_pos =
      let open Docstrings in
        match docs, lines with
        | Initial, _ -> ()
        | After a, (NoLine | NewLine) ->
            set_post_docstrings post_pos (List.rev a);
            set_pre_docstrings pre_pos a;
        | After a, BlankLine ->
            set_post_docstrings post_pos (List.rev a);
            set_pre_extra_docstrings pre_pos (List.rev a)
        | Before(a, f, b), (NoLine | NewLine) ->
            set_post_docstrings post_pos (List.rev a);
            set_post_extra_docstrings post_pos
              (List.rev_append f (List.rev b));
            set_floating_docstrings pre_pos (List.rev f);
            set_pre_extra_docstrings pre_pos (List.rev a);
            set_pre_docstrings pre_pos b
        | Before(a, f, b), BlankLine ->
            set_post_docstrings post_pos (List.rev a);
            set_post_extra_docstrings post_pos
              (List.rev_append f (List.rev b));
            set_floating_docstrings pre_pos
              (List.rev_append f (List.rev b));
            set_pre_extra_docstrings pre_pos (List.rev a)
    in
    let rec loop lines docs lexbuf =
      match token_with_comments lexbuf with
      | COMMENT (s, loc) ->
          add_comment (s, loc);
          let lines' =
            match lines with
            | NoLine -> NoLine
            | NewLine -> NoLine
            | BlankLine -> BlankLine
          in
          loop lines' docs lexbuf
      | EOL ->
          let lines' =
            match lines with
            | NoLine -> NewLine
            | NewLine -> BlankLine
            | BlankLine -> BlankLine
          in
          loop lines' docs lexbuf
      | HASH when at_bol lexbuf -> 
          interpret_directive lexbuf 
            (fun lexbuf -> loop lines docs lexbuf)
            (fun token -> sharp_look_ahead := Some token; HASH)            
      | DOCSTRING doc ->
          Docstrings.register doc;
          add_docstring_comment doc;
          let docs' =
            if Docstrings.docstring_body doc = "/*" then
              match docs with
              | Initial -> Before([], [doc], [])
              | After a -> Before (a, [doc], [])
              | Before(a, f, b) -> Before(a, doc :: b @ f, [])
            else
              match docs, lines with
              | Initial, (NoLine | NewLine) -> After [doc]
              | Initial, BlankLine -> Before([], [], [doc])
              | After a, (NoLine | NewLine) -> After (doc :: a)
              | After a, BlankLine -> Before (a, [], [doc])
              | Before(a, f, b), (NoLine | NewLine) -> Before(a, f, doc :: b)
              | Before(a, f, b), BlankLine -> Before(a, b @ f, [doc])
          in
          loop NoLine docs' lexbuf
      | tok ->
          attach lines docs (lexeme_start_p lexbuf);
          tok
    in
      match !sharp_look_ahead with
      | None -> 
          loop NoLine Initial lexbuf
      | Some token ->
          sharp_look_ahead := None ;
          token

  let init () =
    sharp_look_ahead := None;
    update_if_then_else  Dir_out;
    is_in_string := false;
    comment_start_loc := [];
    comment_list := [];
    match !preprocessor with
    | None -> ()
    | Some (init, _preprocess) -> init ()

  let rec filter_directive pos   acc lexbuf : (int * int ) list =
    match token_with_comments lexbuf with
    | HASH when at_bol lexbuf ->
        (* ^[start_pos]#if ... #then^[end_pos] *)
        let start_pos = Lexing.lexeme_start lexbuf in 
        interpret_directive lexbuf 
          (fun lexbuf -> 
             filter_directive 
               (Lexing.lexeme_end lexbuf)
               ((pos, start_pos) :: acc)
               lexbuf
          
          )
          (fun _token -> filter_directive pos acc lexbuf  )
    | EOF -> (pos, Lexing.lexeme_end lexbuf) :: acc
    | _ -> filter_directive pos  acc lexbuf

  let filter_directive_from_lexbuf lexbuf = 
    List.rev (filter_directive 0 [] lexbuf )

  let set_preprocessor init preprocess =
    escaped_newlines := true;
    preprocessor := Some (init, preprocess)


# 3467 "parsing/lexer.ml"
