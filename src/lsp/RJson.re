
let get = (k, obj) => Json.get(k, obj) |> RResult.orError("No key " ++ k);
let string = (obj) => Json.string(obj) |> RResult.orError("Not a string");
let number = (obj) => Json.number(obj) |> RResult.orError("Not a number");
let array = obj => Json.array(obj) |> RResult.orError("Expected an array");
