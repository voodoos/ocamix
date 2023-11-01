open! Std
open Brr
open Brr_lwd

module Generic = struct
  let make ?(base_classes = Classes.make []) ?d ?(at = [])
      ?(classes = Classes.Add []) ~handlers content =
    let classes = Classes.update base_classes classes |> Classes.to_list in
    let at_classes =
      List.map classes ~f:(fun c -> `P (At.class' @@ Jstr.v c))
    in
    let at = List.rev_append at at_classes in
    Elwd.button ?d ~at ~ev:handlers content
end

module Simple = struct
  (** Makes a simple button with a single [on_click] handler. *)
  let make ~base_classes ?d ?at ?classes ~on_click content =
    Generic.make ~base_classes ?classes ?d ?at ~handlers:[ `P on_click ] content
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

  let make ~config ?d ?classes ?(state = Off) ~on_click content =
    let open Lwd_infix in
    let v_state = Lwd.var state in
    let elt =
      let$* state = Lwd.get v_state in
      let base_classes =
        let classes = get_state_classes config state in
        Classes.union config.base_classes classes
      in
      let handler =
        Elwd.handler Ev.click (fun ev ->
            match on_click state ev with
            | Toggle -> Lwd.set v_state @@ toggle state
            | Set state -> Lwd.set v_state state
            | None -> ())
      in
      Generic.make ~base_classes ?d ?classes ~handlers:[ `P handler ] content
    in
    { elt; force = force v_state }
end
