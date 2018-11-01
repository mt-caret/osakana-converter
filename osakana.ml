open Core_kernel
open Async_kernel
open Js_of_ocaml
open Incr_dom
open Virtual_dom

module Model = struct
  type t = unit [@@deriving compare, sexp]

  let cutoff = [%compare.equal: t]

  let initial_model () = ()
end

module Action = struct
  type t =
    | Nop
  [@@deriving compare, sexp]

  let should_log _ = true
end

module State = struct
  type t = unit
end

let apply_action (action : Action.t) model _state =
  match action with
  | Nop -> model (* nop *)
;;

let update_visibility = Fn.id

let view _model ~inject:_ =
  Incr.const Vdom.Node.(p [] [ text "Hello, World from incr_dom" ])
;;

let log s = Firebug.console##log (Js.string s)

let on_startup ~schedule:_ _model =
  let _ = Dfa.of_string "" in
  log "Hello, World from incr_dom (log)";
  Deferred.unit

let on_display ~old:_ _model _state = ()

(* let error f = Printf.ksprintf (fun s -> Firebug.console##error (Js.string s); failwith s) f
 * let debug f = Printf.ksprintf (fun s -> Firebug.console##log (Js.string s)) f
 * let alert f = Printf.ksprintf (fun s -> Dom_html.window##alert (Js.string s); failwith s) f *)
