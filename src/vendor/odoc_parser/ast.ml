module Path = Paths.Path
module Reference = Paths.Reference
module Identifier = Paths.Identifier
module Comment = Comment

type 'a with_location = 'a Location_.with_location



type reference_kind = [ `Simple | `With_text ]

type inline_element = [
  | `Space
  | `Word of string
  | `Code_span of string
  | `Styled of Comment.style * (inline_element with_location) list
  | `Reference of
      reference_kind * Reference.any * (inline_element with_location) list
  | `Link of string * (inline_element with_location) list
]

type nestable_block_element = [
  | `Paragraph of (inline_element with_location) list
  | `Code_block of string
  | `Example of string * string
  | `Doc of string
  | `Verbatim of string
  | `Modules of Reference.module_ list
  | `List of
    [ `Unordered | `Ordered ] *
    ((nestable_block_element with_location) list) list
]

type tag = [
  | `Author of string
  | `Deprecated of (nestable_block_element with_location) list
  | `Param of string * (nestable_block_element with_location) list
  | `Raise of string * (nestable_block_element with_location) list
  | `Return of (nestable_block_element with_location) list
  | `See of
      [ `Url | `File | `Document ] *
      string *
      (nestable_block_element with_location) list
  | `Since of string
  | `Before of string * (nestable_block_element with_location) list
  | `Version of string
  | `Canonical of Path.module_ * Reference.module_
  | `Inline
  | `Open
  | `Closed
]

type block_element = [
  | nestable_block_element
  | `Heading of int * string option * (inline_element with_location) list
  | `Tag of tag
]

type docs = (block_element with_location) list



type sections_allowed = [ `All | `No_titles | `None ]
