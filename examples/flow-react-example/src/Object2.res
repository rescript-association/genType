export type openObject<'a> = Js.t<{.."foo": int} as 'a>

export foo = (x: openObject<{"foo": int}>) => x

export useProps = props => props["foo"] + 1

export usePropsClosed = (props: {"fooClosed": int}) => props["fooClosed"] + 1

export type t1<'a> = Js.t<'a>

export type t2 = Js.t<Obj.t>
