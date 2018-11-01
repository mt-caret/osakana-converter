open Core_kernel

module State = struct
  type t =
    { name : string
    ; action : [ `Wait | `Go ]
    ; go : int
    ; wait : int
    } [@@deriving bin_io, sexp, compare]

  let create ~name ~action ~go ~wait = { name; action; go; wait }
end

type t = State.t array [@@deriving bin_io, sexp, compare]

let of_labeled_dfa : Labeled_dfa.t -> (t, string) Result.t =
  function
  | [] -> Error "Labeled DFA has no states"
  | states ->
    let open Result.Let_syntax in
    let%bind index_map =
      List.foldi states ~init:(Ok String.Map.empty) ~f:(fun index map state ->
          match%bind map >>| String.Map.add ~key:state.name ~data:index with
          | `Ok x -> Ok x
          | `Duplicate -> Error ("Duplicate state: " ^ state.name))
    in
    let lookup_index state_name =
      match String.Map.find index_map state_name with
      | Some index -> Ok index
      | None -> Error ("State " ^ state_name ^ " not found")
    in
    let%map indexed_list =
      List.map states ~f:(fun state ->
        let%map go = lookup_index state.go
        and wait = lookup_index state.wait in
        State.create ~name:state.name ~action:state.action ~go ~wait)
      |> Result.all
    in
    List.to_array indexed_list
;;

let labeled_dfa_of_string str =
  let lexbuf = Lexing.from_string str in
  Result.try_with (fun () -> Parser.dfa Lexer.read lexbuf)
  |> Result.map_error ~f:Exn.to_string
;;

let of_string str =
  let open Result.Let_syntax in
  let%bind labeled_dfa = labeled_dfa_of_string str in
  of_labeled_dfa labeled_dfa
;;
