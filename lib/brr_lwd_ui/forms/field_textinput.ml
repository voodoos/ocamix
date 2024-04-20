open! Std
open! Brrer
open! Brr
open! Brr_lwd

type label = Elwd.t Elwd.col
type desc = { placeholder : string option Lwd.t; label : label }
type t = { name : string; default : string option; desc : desc }

type 'a reactive_field = {
  field : Elwd.t Lwd.t;
  label : Elwd.t Lwd.t;
  value : 'a Lwd.t;
}

let name ~id base_name =
  if id then Printf.sprintf "%s--id" base_name
  else Printf.sprintf "%s" base_name

let make t =
  let id = name ~id:true t.name in
  let name = name ~id:false t.name in
  let var = Persistent.var ~key:id t.default in
  let field, label =
    let open Lwd_infix in
    let label = Elwd.label ~at:[ `P (At.for' (Jstr.v id)) ] t.desc.label in
    let at =
      let open Attrs in
      add At.Name.id (`P id) []
      |> add At.Name.name (`P name)
      |> add At.Name.type' (`P "text")
    in
    let at =
      match Lwd.peek var with
      | Some v -> `P (At.value @@ Jstr.v v) :: at
      | None -> at
    in
    let on_change =
      Elwd.handler Ev.keyup (fun ev ->
          let t = Ev.target ev |> Ev.target_to_jv in
          let value = Jv.get t "value" in
          Lwd.set var (Some (Jv.to_string value)))
    in
    let ev = [ `P on_change ] in
    (Elwd.(div [ `R (input ~at ~ev ()) ]), label)
  in
  { field; label; value = Lwd.get var }
