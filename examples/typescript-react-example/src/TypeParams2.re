[@genType]
type item = {[@dead "item.id"] id: int};

[@genType]
type items = TypeParams1.ocaml_array(item);

[@genType]
type items2 = array(item);

[@dead "exportSomething"] let exportSomething = 10;