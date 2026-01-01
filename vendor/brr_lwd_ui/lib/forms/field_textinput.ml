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

let make ?(at = []) ?(ev = []) ?placeholder ?debounce
    ?(on_change = fun ~init:_ -> ignore) (desc : string option Field.desc) =
  let id = name ~id:true desc.name in
  let name = name ~id:false desc.name in
  let value = Persistent.var ~key:id desc.default in
  let () = Lwd.peek value |> Option.iter (on_change ~init:true) in
  let label = Elwd.label ~at:[ `P (At.for' (Jstr.v id)) ] desc.label in
  let element = ref None in
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
    let on_change, on_kup =
      let f =
       fun ev ->
        let t = Ev.target ev |> Ev.target_to_jv in
        let value' = Jv.get t "value" |> Jv.to_string in
        on_change ~init:false value';
        Lwd.set value (Some value')
      in
      let debouncer =
        (* TODO This is awful *)
        match debounce with
        | None -> fun f -> f ()
        | Some delay_ms -> Limiter.throttle ~delay_ms ~delay:true
      in
      let f = fun ev -> debouncer (fun () -> f ev) in
      (Elwd.handler Ev.change f, Elwd.handler Ev.keyup f)
    in
    (* TODO: triggering both on change and key up events prevents situations
       were the field loses focus right after pressing a key and then not
       receiving a key up event. However this also causes un-needed triggering
       of the change events when the user deselects the field some time after
       typing.

       See: https://developer.mozilla.org/en-US/docs/Web/API/Element/keyup_event

       > The event target might change between different key events. For
       > example, the keydown target for pressing the Tab key would be different
       > from the keyup target, because the focus has changed.*)
    let ev = `P on_change :: `P on_kup :: ev in
    Elwd.input ~at ~ev ~on_create:(fun e -> element := Some e) ()
  in
  let () =
    (* React to direct control of the var *)
    Utils.tap ~f:(fun v ->
        let text = Option.value ~default:"" v in
        Option.iter
          (fun text_input ->
            Jv.set (El.to_jv text_input) "value" (Jv.of_string text);
            on_change ~init:false text)
          !element)
    @@ Lwd.get value
  in
  { field; label; value }
