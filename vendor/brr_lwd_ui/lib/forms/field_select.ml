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
  if id then Printf.sprintf "%s--id" base_name else base_name

let make ?(persist = true) ?(at = []) ?(ev = [])
    ?(on_change = fun ~init:_ -> ignore) (desc : string Field.desc) options =
  let id = name ~id:true desc.name in
  let name = name ~id:false desc.name in
  let value =
    if persist then Persistent.var ~key:id desc.default
    else Lwd.var desc.default
  in
  let () = on_change ~init:true @@ Lwd.peek value in
  let label = Elwd.label ~at:[ `P (At.for' (Jstr.v id)) ] desc.label in
  let element = ref None in
  let field =
    let at =
      let open Attrs in
      add At.Name.id (`P id) at
      |> add At.Name.name (`P name)
      |> add At.Name.type' (`P "text")
    in
    let on_change =
      Elwd.handler Ev.change (fun ev ->
          let t = Ev.target ev |> Ev.target_to_jv in
          let value' = Jv.get t "value" |> Jv.to_string in
          on_change ~init:false value';
          Lwd.set value value')
    in
    let ev = `P on_change :: ev in
    let options =
      Lwd_seq.map
        (fun (value', name) ->
          let open Attrs.O in
          let at = v (`P (A (At.value @@ Jstr.v value'))) in
          let selected =
            Lwd.map (Lwd.get value) ~f:(fun selected ->
              let selected = String.equal selected value' in
                A (At.if' selected At.selected))
          in
          let at = `R selected @:: at in
          Elwd.option ~at [ `P (El.txt' name) ])
        options
    in
    Elwd.select ~at ~ev ~on_create:(fun e -> element := Some e) [ `S (Lwd_seq.lift options) ]
  in
  let () = Utils.listen ~f:(fun v ->
    Option.iter (fun select ->
    Jv.set (El.to_jv select) "value" (Jv.of_string v)) !element )
     @@ Lwd.get value
  in
  { field; label; value }
