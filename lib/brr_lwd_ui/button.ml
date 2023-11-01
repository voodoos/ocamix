open! Std
open Brr
open Brr_lwd

module Make (Properties : sig
  val base_classes : Classes.t
end) =
struct
  (** Make a button with reactive content. Not that the handler itself is not reactive. *)
  let make ?d ?(classes = Classes.Add []) ~on_click content =
    let classes =
      Classes.update Properties.base_classes classes |> Classes.to_list
    in
    let at = List.map classes ~f:(fun c -> `P (At.class' @@ Jstr.v c)) in
    let handler = Elwd.handler Ev.click on_click in
    let button = Elwd.button ?d ~at ~ev:[ `P handler ] content in
    button

  (** Make a pure button. *)
  let make_pure ?d ?(classes = Classes.Add []) ~on_click content =
    let classes =
      Classes.update Properties.base_classes classes |> Classes.to_list
    in
    let at = List.map classes ~f:(fun c -> At.class' @@ Jstr.v c) in
    let button = El.button ?d ~at content in
    let listener = Ev.listen Ev.click on_click (El.as_target button) in
    (button, listener)
end

module Two_state = struct
  type config = {
    base_classes : Classes.t;
    on_classes : Classes.t;
    off_classes : Classes.t;
  }

  type state = On | Off
  type action = None | Toggle | Set of state
  type t = { elt : El.t Lwd.t; force : action -> unit }

  let toggle = function Off -> On | On -> Off

  let get_state_classes config = function
    | On -> config.on_classes
    | Off -> config.off_classes

  let force var = function
    | Set state -> Lwd.set var state
    | Toggle -> Lwd.set var (toggle @@ Lwd.peek var)
    | None -> ()

  let make ~config ?d ?(classes = Classes.Add []) ?(state = Off) ~on_click
      content =
    let open Lwd_infix in
    let v_state = Lwd.var state in
    let base_classes = Classes.update config.base_classes classes in
    let elt =
      let$* state = Lwd.get v_state in
      let at =
        let classes = get_state_classes config state in
        Classes.union base_classes classes
        |> Classes.to_list
        |> List.map ~f:(fun c -> Utils.pure @@ At.class' (Jstr.v c))
      in
      let handler =
        Elwd.handler Ev.click (fun ev ->
            match on_click state ev with
            | Toggle -> Lwd.set v_state @@ toggle state
            | Set state -> Lwd.set v_state state
            | None -> ())
      in
      Elwd.button ?d ~at ~ev:[ `P handler ] content
    in
    { elt; force = force v_state }
end
