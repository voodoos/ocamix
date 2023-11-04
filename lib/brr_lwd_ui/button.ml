open! Std
open Brr
open Brr_lwd

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
end

let apply_state state f = f state

let with_state (type t) (module S : State with type t = t) ?(state = S.default)
    ?d ?(at : (S.t -> At.t Elwd.col) option)
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
    let at = Option.map (apply_state state) at in
    let ev =
      Option.map
        (List.map ~f:(function
          | `P h -> `P (with_state h)
          | `R h -> `R (Lwd.map h ~f:with_state)
          | `S h -> `S (Lwd_seq.map with_state h)))
        ev
    in
    Elwd.button ?d ?at ?ev (content state)
  in
  (elt, get_state, set_state)

module Two_state = struct
  type t = On | Off

  let to_string = function On -> "On" | Off -> "Off"
  let default = On
  let next = function On -> Off | Off -> On
end

(* let make content =
   button
     (module Two_state)
     ~at:(function
       | Two_state.On -> [ `P (At.class' @@ Jstr.v "on") ] | Off -> [])
     ~ev:
       [
         on_click (fun s _ ->
             match s with
             | Two_state.On ->
                 Console.log [ "STATE IS ON" ];
                 Set Off
             | Off ->
                 Console.log [ "STATE IS Off" ];
                 Set On);
       ]
     (fun t -> `P (Two_state.to_string t |> El.txt') :: content) *)

let two_state = with_state (module Two_state)
