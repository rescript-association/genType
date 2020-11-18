
type name = string
type url = string
type title = string
type alt = string

type 'a el =
  [ `Text of string
  | `Br
  | `Emph of 'a
  | `Bold of 'a
  | `Url of url * 'a * title
  | `Img of url * alt * title
  | `Code of name * string
  | `Html of string
  | `Comment of string
  ]

type phrasing_no_NL = phrasing_no_NL el list

type phrasing =  [phrasing el | `NL] list

type reference

type flow =
  [ phrasing el
  | `H1 of phrasing_no_NL
  | `H2 of phrasing_no_NL
  | `H3 of phrasing_no_NL
  | `H4 of phrasing_no_NL
  | `H5 of phrasing_no_NL
  | `H6 of phrasing_no_NL
  | `Hr
  | `Paragraph of phrasing
  | `Code_block of name * string
  | `Html_block of string
  | `Ul of t
  | `Ol of t
  | `Quote of t
  | `Ref of reference
  | `Img_ref of reference
  ]

and t = flow list
