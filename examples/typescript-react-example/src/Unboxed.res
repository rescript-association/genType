@ocaml.unboxed
export type v1 = A(int)

@unboxed
export type v2 = A(int)

export testV1 = (x: v1) => x

@unboxed
export type r1 = {x: int}

@ocaml.unboxed
export type r2 = B({g: string})

export r2Test = (x: r2) => x
