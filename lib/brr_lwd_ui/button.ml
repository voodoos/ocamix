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

type 'a update = None | Next | Set of 'a

type 'state handler_with_state =
  | Handler_with_state : {
      opts : Ev.listen_opts option;
      type' : 'a Ev.type';
      func : 'state -> 'a Ev.t -> 'state update;
    }
      -> 'state handler_with_state

let to_handler state (Handler_with_state { opts; type'; func }) =
  let func ev = ignore @@ func state ev in
  Elwd.handler ?opts type' func

let handler_with_state ?opts type' func =
  Handler_with_state { opts; type'; func }

let on_click ?opts f = `P (handler_with_state ?opts Ev.click f)

module type State = sig
  type t

  val default : t
  val next : t -> t
  val style : t -> At.t Elwd.col
end

let button (type t) (module S : State with type t = t) ?(state = S.default) ?d
    ?at ?(ev : t handler_with_state Elwd.col option)
    (content : S.t -> El.t Elwd.col) =
  let v_state = Lwd.var state in
  let get_state () = Lwd.get v_state in
  let set_state t = Lwd.set v_state t in
  let with_state state (Handler_with_state { opts; type'; func }) =
    let func ev =
      match func state ev with
      | None -> ()
      | Set s -> set_state s
      | Next -> set_state @@ S.next (Lwd.peek v_state)
    in
    Elwd.handler ?opts type' func
  in
  let elt =
    let open Lwd_infix in
    let$* state = get_state () in
    let with_state = with_state state in
    let state_at = S.style state in
    let at = Option.map_or ~default:state_at (List.rev_append state_at) at in
    let ev =
      Option.map
        (List.map ~f:(function
          | `P h -> `P (with_state h)
          | `R h -> `R (Lwd.map h ~f:with_state)
          | `S h -> `S (Lwd_seq.map with_state h)))
        ev
    in
    Elwd.button ?d ~at ?ev (content state)
  in
  (elt, get_state, set_state)

type ts_state = On | Off

module TS : State with type t = ts_state = struct
  type t = ts_state

  let default = On
  let next = function On -> Off | Off -> On
  let style = function On -> [ `P (At.class' @@ Jstr.v "on") ] | Off -> []
end

let make content =
  button
    (module TS)
    ~ev:
      [
        on_click (fun s _ ->
            match s with
            | On ->
                Console.log [ "STATE IS ON" ];
                Set Off
            | Off ->
                Console.log [ "STATE IS Off" ];
                Set On);
      ]
    (fun _ -> content)
