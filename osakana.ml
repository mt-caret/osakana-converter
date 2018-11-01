open Core_kernel
open Async_kernel
open Js_of_ocaml
open Incr_dom
open Virtual_dom

module Model = struct
  type t =
    { labeled_dfa_field : string
    ; conversion_result : string
    ; error_message : string option
    } [@@deriving compare, sexp, fields]

  let cutoff = [%compare.equal: t]

  let initial_model () =
    { labeled_dfa_field = ""
    ; conversion_result = ""
    ; error_message = None
    }
  ;;
end

module Action = struct
  type t =
    | Nop
    | Update_dfa_field of string
    | Conversion_success of string
    | Show_error of string
  [@@deriving compare, sexp, variants]

  let should_log _ = false
end

module State = struct
  type t = unit
end

let log s = Firebug.console##log (Js.string s)

let apply_action (action : Action.t) (model : Model.t) _state =
  sprintf !"%{sexp:Action.t}" action |> log;
  match action with
  | Nop -> model
  | Update_dfa_field labeled_dfa_field ->
    { model with
      labeled_dfa_field
    }
  | Conversion_success conversion_result ->
    { model with
      conversion_result
    ; error_message = None
    }
  | Show_error error_message ->
    { model with
      error_message = Some error_message
    }
;;

let update_visibility = Fn.id

let view (model : Model.t Incr.t) ~inject =
  let open Vdom in
  let open Vdom.Node in
  let open Incr.Let_syntax in
  let%map labeled_dfa_field = model >>| Model.labeled_dfa_field in
  div
    []
    [ label [ Attr.for_ "labeled_dfa_field" ] [ text "Labeled DFA:" ]
    ; textarea
        [ Attr.id "labeled_dfa_field"
        ; Attr.on_input (fun _ev -> Fn.compose inject Action.update_dfa_field)
        ]
        [ text labeled_dfa_field ]
    ; label [ Attr.for_ "conversion_result_field" ] [ text "Conversion Result:" ]
    ; textarea
        [ Attr.id "conversion_result"
        ; Attr.on_keydown (fun ev ->
            Dom.preventDefault ev;
            inject Action.Nop)
        ]
        [ text labeled_dfa_field ]
    ]
  (* Vdom.Node.(p [] [ text "Hello, World from incr_dom" ]) *)
;;


let on_startup ~schedule:_ _model =
  let _ = Dfa.of_labeled_dfa_string "" in
  log "Hello, World from incr_dom (log)";
  Deferred.unit

let on_display ~old:_ _model _state = ()

(* let error f = Printf.ksprintf (fun s -> Firebug.console##error (Js.string s); failwith s) f
 * let debug f = Printf.ksprintf (fun s -> Firebug.console##log (Js.string s)) f
 * let alert f = Printf.ksprintf (fun s -> Dom_html.window##alert (Js.string s); failwith s) f *)
