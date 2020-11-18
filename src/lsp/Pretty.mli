(**
   Pretty-prints documents.  This module is similar in purpose to
   OCaml standard Format module, but is based on the combinators
   described in Wadler's paper
   {{:http://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf}
   "A prettier printer."}
   Unlike Wadler's implementation, the present code does not admit
   lazy document nodes and therefore pretty printing always uses O(N)
   space where N is the size of the document.
   {e References}
   Philip Wadler,
   {{:http://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf}
   "A prettier printer."} The Fun of Programming. A symposium in
   honour of Professor Richard Bird's 60th birthday Examination
   Schools, Oxford, 24-25 March 2003.
*)

(** Represents documents to be pretty-printed *)
type doc

(** Sequentially appends two documents *)
val append : doc -> doc -> doc

(** The empty document *)
val empty : doc

(** Constructs a group based on a given document. The printer has two
    ways to print a group: it either prints exactly as the defining
    document, or prints as its flattened version where all newlines
    are collapsed.  The choice is made by the printer based on
    available space. *)
val group : doc -> doc

(** Adds a level of indentation to a document *)
val indent : int -> doc -> doc

(** Constructs a primitive document that prints as a newline unless
    the printer chooses to flatten the enclosing group.  In the later
    case, the document prints as a given string. *)
val line : string -> doc

(** Constructs a primitive document that prints as the given string.
    For best results, the string should not contain newlines *)
val text : ?len: int -> string -> doc

(** Constructs a linebreak that dedents *)
val back : int -> string -> doc

(** Pretty-prints the document.  You can override the device width,
    the "output" function that defines how to output text, and the
    "indent" function that defines how to print a newline and indent
    to a desired level.  The default arguments print to stdout. *)
val print : ?width: int ->
  ?output: (string -> unit) ->
  ?indent: (int -> unit) ->
  doc ->
  unit
