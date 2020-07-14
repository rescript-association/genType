export type record = {
  x: int,
  y: string,
}

let r: record

@genType
module type MT = {
  let x: int
}

export type firstClassModule = module(MT)
