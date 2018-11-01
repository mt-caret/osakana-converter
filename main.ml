open! Core_kernel
open! Js_of_ocaml

let () =
  Incr_dom.Start_app.simple
    ~bind_to_element_with_id:"app"
    ~initial_model:(Osakana.Model.initial_model ())
    (module Osakana)
;;
