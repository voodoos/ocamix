open! Import
open! Brrer
open! Brr
open! Brr_lwd

type 'a reactive_field = {
  field : Elwd.t Lwd.t;
  label : Elwd.t Lwd.t;
  value : 'a Lwd.var;
}

let name ~id base_name =
  if id then Printf.sprintf "%s--id" base_name
  else Printf.sprintf "%s" base_name

let make ?(at = []) ?(ev = []) ?placeholder ?(on_change = fun ~init:_ -> ignore)
    (desc : string option Field.desc) =
  let id = name ~id:true desc.name in
  let name = name ~id:false desc.name in
  let value = Persistent.var ~key:id desc.default in
  let () = Lwd.peek value |> Option.iter (on_change ~init:true) in
  let label = Elwd.label ~at:[ `P (At.for' (Jstr.v id)) ] desc.label in
  let field =
    let at =
      let open Attrs in
      add At.Name.id (`P id) at
      |> add At.Name.name (`P name)
      |> add At.Name.type' (`P "text")
      |> add_opt At.Name.placeholder placeholder
    in
    let at =
      match Lwd.peek value with
      | Some v -> `P (At.value @@ Jstr.v v) :: at
      | None -> at
    in
    let on_change =
      Elwd.handler Ev.keyup (fun ev ->
          let t = Ev.target ev |> Ev.target_to_jv in
          let value' = Jv.get t "value" |> Jv.to_string in
          on_change ~init:false value';
          Lwd.set value (Some value'))
    in
    let ev = `P on_change :: ev in
    Elwd.input ~at ~ev ()
  in
  { field; label; value }
