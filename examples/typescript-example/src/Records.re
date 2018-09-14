open Belt;

[@genFlow]
type coord = {
  x: int,
  y: int,
  z: option(int),
};

[@genFlow]
let origin = {x: 0, y: 0, z: Some(0)};

[@genFlow]
let computeArea = ({x, y, z}) =>
  Option.(x * y * z->(mapWithDefault(1, n => n)));

[@genFlow]
let coord2d = (x, y) => {x, y, z: None};

[@genFlow]
type person = {
  name: string,
  age: int,
  address: option(string),
};

[@genFlow]
type business = {
  name: string,
  owner: option(person),
  address: option(string),
};

let getOpt = (opt, default, foo) => opt->Option.mapWithDefault(default, foo);

[@genFlow]
let findAddress = (business: business): list(string) =>
  business.address->getOpt([], a => [a]);

[@genFlow]
let findAllAddresses = (businesses: array(business)): array(string) =>
  businesses
  ->Array.map(business =>
      business.address->getOpt([], a => [a])
      @ business.owner->getOpt([], p => p.address->getOpt([], a => [a]))
    )
  ->List.fromArray
  ->List.flatten
  ->List.toArray;