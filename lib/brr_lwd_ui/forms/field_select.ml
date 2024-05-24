open! Std
open! Brrer
open! Brr
open! Brr_lwd

type 'a reactive_field = {
  field : Elwd.t Lwd.t;
  label : Elwd.t Lwd.t;
  value : 'a Lwd.t;
}

let name ~id base_name =
  if id then Printf.sprintf "%s--id" base_name else base_name

let make ?(at = []) ?(ev = []) (desc : string Field.desc) options =
  let id = name ~id:true desc.name in
  let name = name ~id:false desc.name in
  let var = Persistent.var ~key:id desc.default in
  let label = Elwd.label ~at:[ `P (At.for' (Jstr.v id)) ] desc.label in
  let field =
    let open Lwd_infix in
    let at =
      let open Attrs in
      add At.Name.id (`P id) at
      |> add At.Name.name (`P name)
      |> add At.Name.type' (`P "text")
    in
    let on_change =
      Elwd.handler Ev.change (fun ev ->
          let t = Ev.target ev |> Ev.target_to_jv in
          let value = Jv.get t "value" in
          Lwd.set var (Jv.to_string value))
    in
    let ev = `P on_change :: ev in
    let options =
      Lwd_seq.map
        (fun (value, name) ->
          let open Attrs.O in
          let at = v (`P (A (At.value @@ Jstr.v value))) in
          let selected =
            Lwd.map (Lwd.get var) ~f:(fun selected ->
                A (At.if' (Equal.poly selected value) At.selected))
          in
          let at = `R selected @:: at in
          Elwd.option ~at [ `P (El.txt' name) ])
        options
    in
    Elwd.select ~at ~ev [ `S (Lwd_seq.lift options) ]
  in
  { field; label; value = Lwd.get var }
