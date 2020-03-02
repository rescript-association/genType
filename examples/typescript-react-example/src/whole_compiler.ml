module Misc : sig 
  module type HookSig = sig
    type t
    val add_hook : string -> unit
  end

  module MakeHooks : HookSig with type t = int
end = struct
  module type HookSig = sig
    type t
    val add_hook : string -> unit
  end

  module MakeHooks : HookSig with type t = int
  = struct
    type t = int
    let add_hook _ = assert false
  end
end
