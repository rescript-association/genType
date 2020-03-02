module QQ =
  struct
    let thisSpansSeveralLines =
      (fun x -> fun y ->
          x + y : int -> int -> int)
  end

module AA : sig
  val thisIsInInterface :
    int -> int
end = struct
  let thisIsInInterface x = x
end

let thisHasSemicolons = 3;;
