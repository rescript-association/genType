type person = {
  name: string,
  age: int,
  optional: option<float>,
  unknown: option<list<int>>,
}

@react.component
export make = (~array, ~callback=() => (), ~person, ~title) => {
  callback()
  <div>
    {React.string(
      "Test Component Title:" ++
      (title ++
      (" Name:" ++ (person.name ++ (" array[0]:" ++ array[0])))),
    )}
  </div>
}

export poly = (x, _y) => x + 1

export default = make
