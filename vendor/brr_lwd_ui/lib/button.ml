open! Import
open Brr
open Brr_lwd

let v ?d ?(at = []) ?ev value =
  let at =
    Attrs.add At.Name.type' (`P "button") at |> Attrs.add At.Name.value value
  in
  Elwd.input ?d ~at ?ev ()

type 'a update = None | Next | Set of 'a

type 'state handler_with_state =
  | Handler_with_state : {
      opts : Ev.listen_opts option;
      type' : 'a Ev.type';
      func : 'state -> 'a Ev.t -> 'state update;
    }
      -> 'state handler_with_state

let handler ?opts type' func = Handler_with_state { opts; type'; func }

module type State = sig
  type t

  val default : t
  val next : t -> t
end

let apply_state state f = f state

let with_state ?(base = Attrs.empty) (type t) (module S : State with type t = t)
    ?(state = S.default) ?d ?(at : (S.t -> Attrs.t) option)
    ?(ev : t handler_with_state Elwd.col option)
    (content : S.t -> El.t Elwd.col) =
  let v_state = Lwd.var state in
  let get_state () = Lwd.get v_state in
  let set_state t = Lwd.set v_state t in
  let elt =
    let open Lwd_infix in
    let$* state = get_state () in
    let with_state (Handler_with_state { opts; type'; func }) =
      let func ev =
        match func state ev with
        | None -> ()
        | Set s -> set_state s
        | Next -> set_state @@ S.next state
      in
      Elwd.handler ?opts type' func
    in
    let at =
      Option.map_or ~default:base
        (fun at -> Attrs.union base @@ apply_state state at)
        at
    in
    let at = Attrs.to_at at in
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

type two_state = On | Off

module Two_state = struct
  type t = two_state

  let default = On
  let next = function On -> Off | Off -> On
end

let two_state ?base = with_state ?base (module Two_state)
