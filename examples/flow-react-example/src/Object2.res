@genType type openObject<'a> = Js.t<{.."foo": int} as 'a>

@genType let foo = (x: openObject<{"foo": int}>) => x

@genType let useProps = props => props["foo"] + 1

@genType let usePropsClosed = (props: {"fooClosed": int}) => props["fooClosed"] + 1

@genType type t1<'a> = Js.t<'a>

@genType type t2 = Js.t<Obj.t>
