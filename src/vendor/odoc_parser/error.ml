type full_location_payload = {
  location : Location_.span;
  message : string;
}

type filename_only_payload = {
  file : string;
  message : string;
}

type t = [
  | `With_full_location of full_location_payload
  | `With_filename_only of filename_only_payload
]

type 'a with_warnings = {
  result : 'a;
  warnings : t list;
}

let make : string -> Location_.span -> t = fun message location ->
  `With_full_location {location; message}

let filename_only : string -> string -> t = fun message file ->
  `With_filename_only {file; message}

let format = fun format ->
  (Printf.ksprintf make) format

let to_string : t -> string = function
  | `With_full_location {location; message} ->
    let location_string =
      if location.start.line = location.end_.line then
        Printf.sprintf "line %i, characters %i-%i"
          location.start.line
          location.start.column
          location.end_.column
      else
        Printf.sprintf "line %i, character %i to line %i, character %i"
          location.start.line
          location.start.column
          location.end_.line
          location.end_.column
    in
    Printf.sprintf "File \"%s\", %s:\n%s" location.file location_string message

  | `With_filename_only {file; message} ->
    Printf.sprintf "File \"%s\":\n%s" file message

exception Conveyed_by_exception of t

type ('a, 'b) result = | Ok of 'a | Error of 'b

let raise_exception : t -> _ = fun error ->
  raise (Conveyed_by_exception error)

let to_exception : ('a, t) result -> 'a = function
  | Ok v -> v
  | Error error -> raise_exception error

let catch : (unit -> 'a) -> ('a, t) result = fun f ->
  try Ok (f ())
  with Conveyed_by_exception error -> Error error

(* TODO This is a temporary measure until odoc is ported to handle warnings
   throughout. *)
let shed_warnings : 'a with_warnings -> 'a = fun with_warnings ->
  with_warnings.warnings
  |> List.iter (fun warning -> warning |> to_string |> prerr_endline);
  with_warnings.result
