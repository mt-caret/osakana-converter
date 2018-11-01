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
  [@@deriving compare, sexp, variants]

  let should_log _ = false
end

module State = struct
  type t = unit
end

let apply_action (action : Action.t) (model : Model.t) _state =
  sprintf !"%{sexp:Action.t}" action |> Util.log;
  match action with
  | Nop -> model
  | Update_dfa_field labeled_dfa_field ->
    let new_model = { model with labeled_dfa_field } in
    match Dfa.of_labeled_dfa_string labeled_dfa_field with
    | Ok dfa ->
      let conversion_result = Dfa.to_string dfa in
      { new_model with
        conversion_result
      ; error_message = None
      }
    | Error error_message ->
      { new_model with
        error_message = Some error_message
      }
;;

let update_visibility = Fn.id

let view (model : Model.t Incr.t) ~inject =
  let open Vdom in
  let open Vdom.Node in
  let open Incr.Let_syntax in
  let%map labeled_dfa_field = model >>| Model.labeled_dfa_field
  and conversion_result = model >>| Model.conversion_result
  and error_message = model >>| Model.error_message in
  let error_message_text = Option.map ~f:text error_message in
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
        [ text conversion_result ]
    ; span [] (Option.to_list error_message_text)
    ]
;;

let on_startup ~schedule:_ _model =
  let _ = Dfa.of_labeled_dfa_string "" in
  Util.log "Hello, World from incr_dom (log)";
  Deferred.unit

let on_display ~old:_ _model _state = ()

(* let error f = Printf.ksprintf (fun s -> Firebug.console##error (Js.string s); failwith s) f
 * let debug f = Printf.ksprintf (fun s -> Firebug.console##log (Js.string s)) f
 * let alert f = Printf.ksprintf (fun s -> Dom_html.window##alert (Js.string s); failwith s) f *)
