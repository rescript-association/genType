type point = {
  line : int;
  column : int;
}

type span = {
  file : string;
  start : point;
  end_ : point;
}

type +'a with_location = {
  location : span;
  value : 'a;
}

let at : span -> 'a -> 'a with_location = fun location value ->
  {location; value}

let location : 'a with_location -> span = fun {location; _} ->
  location

let value : 'a with_location -> 'a = fun {value; _} ->
  value

let map : ('a -> 'b) -> 'a with_location -> 'b with_location =
    fun f annotated ->
  {annotated with value = f annotated.value}

let same : _ with_location -> 'b -> 'b with_location = fun annotated value ->
  {annotated with value}

let span : span list -> span = fun spans ->
  match spans with
  | [] ->
    {
      file = "_none_";
      start = {
        line = 1;
        column = 0;
      };
      end_ = {
        line = 1;
        column = 0;
      };
    }
  | first::spans ->
    let last = List.fold_left (fun _ span -> span) first spans in
    {
      file = first.file;
      start = first.start;
      end_ = last.end_;
    }
