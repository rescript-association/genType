module Path = Paths.Path
module Reference = Paths.Reference
module Identifier = Paths.Identifier

type 'a with_location = 'a Location_.with_location



type style = [
  | `Bold
  | `Italic
  | `Emphasis
  | `Superscript
  | `Subscript
]

type leaf_inline_element = [
  | `Space
  | `Word of string
  | `Code_span of string
]

type non_link_inline_element = [
  | leaf_inline_element
  | `Styled of style * (non_link_inline_element with_location) list
]

(* The cross-referencer stores section heading text, and sometimes pastes it
   into link contents. This type alias is provided for use by the
   cross-referencer. *)
type link_content = (non_link_inline_element with_location) list

type inline_element = [
  | leaf_inline_element
  | `Styled of style * (inline_element with_location) list
  | `Reference of Reference.any * link_content
  | `Link of string * link_content
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

type heading_level = [
  | `Title
  | `Section
  | `Subsection
  | `Subsubsection
]

type block_element = [
  | nestable_block_element
  | `Heading of heading_level * Identifier.label * link_content
  | `Tag of tag
]

type docs = (block_element with_location) list

type docs_or_stop = [
  | `Docs of docs
  | `Stop
]
