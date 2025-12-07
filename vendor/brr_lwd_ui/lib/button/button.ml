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

let with_state ?(base = []) (type t) (module S : State with type t = t)
    ?(state = Lwd.var S.default) ?control ?d ?(at : (S.t -> At.t) option)
    ?(ev : t handler_with_state Elwd.col option)
    (content : S.t -> Elwd.t Elwd.col) =
  let v_state = state in
  let () =
    Option.iter (Utils.tap ~initial_trigger:true ~f:(Lwd.set v_state)) control
  in
  let get_state () = Lwd.get v_state in
  let set_state t = Lwd.set v_state t in
  let elt =
    let with_state (Handler_with_state { opts; type'; func }) =
      let func ev =
        let state = Lwd.peek v_state in
        match func state ev with
        | None -> ()
        | Set s -> set_state s
        | Next -> set_state @@ S.next state
      in
      Elwd.handler ?opts type' func
    in
    let at =
      match at with
      | None -> base
      | Some f -> `R (Lwd.map (get_state ()) ~f) :: base
    in
    let ev =
      Option.map
        (List.map ~f:(function
          | `P h -> `P (with_state h)
          | `R h -> `R (Lwd.map h ~f:with_state)
          | `S h -> `S (Lwd_seq.map with_state h)))
        ev
    in
    let content = Lwd.bind (get_state ()) ~f:(fun s -> Elwd.div (content s)) in
    Elwd.button ?d ~at ?ev [ `R content ]
  in
  (elt, get_state, set_state)

type two_state = On | Off

module Two_state = struct
  type t = two_state

  let default = On
  let next = function On -> Off | Off -> On
end

let two_state ?base = with_state ?base (module Two_state)
