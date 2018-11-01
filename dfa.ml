open Core_kernel

module State = struct
  type t =
    { action : [ `Wait | `Go ]
    ; go : int
    ; wait : int
    } [@@deriving bin_io, sexp, compare]

  let create ~action ~go ~wait = { action; go; wait }
end

type t = State.t array [@@deriving bin_io, sexp, compare]

let of_labeled_dfa : Labeled_dfa.t -> (t, string) Result.t =
  function
  | [] -> Error "Labeled DFA has no states"
  | states ->
    let open Result.Let_syntax in
    let%bind index_map =
      List.foldi
        states
        ~init:(Ok String.Map.empty)
        ~f:(fun index map state ->
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
        State.create ~action:state.action ~go ~wait)
      |> Result.all
    in
    List.to_array indexed_list
;;

let labeled_dfa_of_string str =
  let to_position (lexbuf : Lexing.lexbuf) =
    let pos = lexbuf.lex_curr_p in
    sprintf
      "Line %d column %d"
      pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol + 1)
  in
  let lexbuf = Lexing.from_string str in
  Result.try_with (fun () -> Parser.dfa Lexer.read lexbuf)
  |> Result.map_error ~f:(function
      | Lexer.SyntaxError message ->
        sprintf "%s: %s" (to_position lexbuf) message
      | Parser.Error ->
        sprintf "%s: Syntax error" (to_position lexbuf)
      | exn -> Exn.to_string exn)
;;

let of_labeled_dfa_string str =
  let open Result.Let_syntax in
  let%bind labeled_dfa = labeled_dfa_of_string str in
  of_labeled_dfa labeled_dfa
;;

(* let of_string str =
 *   let _assoc_list =
 *     String.split_lines str
 *     |> List.map ~f:(fun line ->
 *       Scanf.sscanf line "%d:%c,%d,%d" (fun index wg go wait ->
 *         let action =
 *           match wg with
 *           | 'w' -> `Wait
 *           | 'g' -> `Go
 *           | _ -> failwith "wrong character"
 *         in
 *         (index, State.create ~action ~go ~wait)))
 *     |> List.sort ~compare:[%compare: int * State.t]
 *   in
 *   failwith "unimplemented" *)

let to_string (t : t) =
  Array.mapi t ~f:(fun index state ->
      let wg =
        match state.action with
        | `Wait -> 'w'
        | `Go -> 'g'
      in
      sprintf "%d:%c,%d,%d" index wg state.go state.wait)
  |> String.concat_array ~sep:"\n"
;;
