(** Taken with modifications from https://github.com/t0yv0/ocaml-pretty/blob/master/pretty.ml *)

type doc = {

  (** The document node *)
  node : node;

  (** Document size when flattened *)
  flat_size : int;

  (** Minimal width of the first line *)
  min_width : int;

  (** True if document contains no newline nodes *)
  single_line : bool;
}

and node =
  | Append of doc * doc
  | Empty
  | Group of doc
  | Indent of int * doc
  | BackLine of int * string
  | Line of int * string (* int = String.length string *)
  | Text of int * string (* int = String.length string *)

let append left right =
  match left.node, right.node with
    | Empty, _ -> right
    | _, Empty -> left
    | _ ->
      {
        node = Append (left, right);
        flat_size = left.flat_size + right.flat_size;
        min_width =
          if left.single_line
          then left.min_width + right.min_width
          else left.min_width;
        single_line = left.single_line && right.single_line;
      }

let empty =
  {
    node = Empty;
    flat_size = 0;
    min_width = 0;
    single_line = true;
  }

let back num text =
  {
    node = BackLine(num, text);
    flat_size = 0;
    min_width = 0;
    single_line = true;
  }

let group doc =
  { doc with node = Group doc }

let indent amount doc =
  { doc with node = Indent (amount, doc) }

let line defaultString =
  let length = String.length defaultString in
  {
    node = Line (length, defaultString);
    flat_size = length;
    min_width = 0;
    single_line = false;
  }

let text ?len string =
  let len = match len with | None -> String.length string | Some n -> n in
  {
    node = Text (len, string);
    flat_size = len;
    min_width = len;
    single_line = true;
  }

let print_indentation n =
  print_char '\n';
  for _ = 1 to n do
  print_char ' ';
  done

let rec flatten doc =
  match doc.node with
    | Append (a, b) -> append (flatten a) (flatten b)
    | Empty | Text _ -> doc
    | Group x | Indent (_, x) -> flatten x
    | Line (_, x) -> text x
    | BackLine (_, x) -> text x

type stack_node =
    {
      doc: doc;
      min_total: int;
      offset: int;
    }

type stack =
  | Nil
  | Cons of stack_node * stack

let min_total stack =
  match stack with
    | Nil -> 0
    | Cons (head, _) -> head.min_total

let push offset node (stack: stack) =
  let current_min_total =
    if node.single_line
    then min_total stack + node.min_width
    else node.min_width in
  Cons ({ doc = node; offset = offset; min_total = current_min_total }, stack)

let print
    ?width:(width=70)
    ?output:(output=print_string)
    ?indent:(indent=print_indentation)
    doc =
  let rec loop currentIndent stack =
    match stack with
      | Nil -> ()
      | Cons (stackNode, rest) ->
        let offset = stackNode.offset in
        match stackNode.doc.node with
          | Append (left, right)  ->
            loop currentIndent (push offset left (push offset right rest))
          | Empty ->
            loop currentIndent rest
          | Group doc ->
            let flatDoc =
              if doc.flat_size + min_total rest <= width - currentIndent
              then flatten doc
              else doc in
            loop currentIndent (push offset flatDoc rest)
          | Indent (ident, doc) ->
            loop currentIndent (push (offset + ident) doc rest)
          | BackLine (num, _) ->
            indent (offset - num);
            loop (offset - num) rest
          | Line _ ->
            indent offset;
            loop offset rest
          | Text (len, string) ->
            output string;
            loop (currentIndent + len) rest in
  loop 0 (push 0 doc Nil)
