open Belt

export testTuple = ((a, b)) => a + b

export type coord = (int, int, option<int>)

export origin = (0, 0, Some(0))

export computeArea = ((x, y, z)) => {
  open Option
  x * y * z->mapWithDefault(1, n => n)
}

export computeAreaWithIdent = ((x, y, z): coord) => {
  open Option
  x * y * z->mapWithDefault(1, n => n)
}

export computeAreaNoConverters = ((x: int, y: int)) => x * y

export coord2d = (x, y) => (x, y, None)

export type coord2 = (int, int, Js.Nullable.t<int>)

export type person = {
  name: string,
  age: int,
}

export type couple = (person, person)

export getFirstName = ((first, _second): couple) => first.name

export marry = (first, second): couple => (first, second)

export changeSecondAge = ((first, second): couple): couple => (
  first,
  {...second, age: second.age + 1},
)
