[@genType]
type openObject('a) = Js.t({.. foo: int} as 'a);

[@genType]
let foo = (x: openObject({. foo: int})) => x;

[@genType]
let useProps = props => props##foo + 1;

[@genType]
let usePropsClosed = (props: {. "fooClosed": int}) => props##fooClosed + 1;