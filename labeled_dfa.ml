open Core_kernel

module State = struct
  type t =
    { name : string
    ; action : [ `Wait | `Go ]
    ; go : string
    ; wait : string
    } [@@deriving bin_io, sexp, compare]

  let create ~name ~action ~go ~wait = { name; action; go; wait }
end

type t = State.t list [@@deriving bin_io, sexp, compare]
