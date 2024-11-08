open! Import
open! Brr
open! Brr_lwd

type checked = bool
type label = Elwd.t Elwd.col
type 'value desc = Check of 'value * label * checked
(* TODO | Group of label * 'value desc list *)

type 'value group = { name : string; desc : 'value desc Lwd_seq.t Lwd.t }
type 'a reactive_field = { field : Elwd.t Lwd.t; value : 'a Lwd.t }

let name ~g ~n base_name = Printf.sprintf "%s-%i-%i" base_name g n

let make_single ?persist ?(ev = []) ?(on_change = fun _ -> ()) name value label
    checked =
  let id = Format.sprintf "%s-id" name in
  let result checked = if checked then Some value else None in
  let var =
    match persist with
    | Some true -> Persistent.var ~key:id (result checked)
    | Some false | None -> Lwd.var (result checked)
  in
  let lbl = Elwd.label ~at:[ `P (At.for' (Jstr.v id)) ] label in
  let at =
    let open Attrs in
    add At.Name.id (`P id) []
    |> add At.Name.name (`P name)
    |> add At.Name.type' (`P "checkbox")
  in
  let checked =
    Lwd.map (Lwd.get var) ~f:(function Some _ -> At.checked | None -> At.void)
  in
  let at = `R checked :: at in
  let on_change =
    Elwd.handler Ev.change (fun ev ->
        let t = Ev.target ev |> Ev.target_to_jv in
        let checked = Jv.get t "checked" in
        let result = result (Jv.to_bool checked) in
        on_change result;
        Lwd.set var result)
  in
  let ev = `P on_change :: ev in
  (Elwd.(div [ `R (input ~at ~ev ()); `R lbl ]), var)

let make ?at ?(persist = true) t =
  (* <fieldset><legend> *)
  (* <fieldset><legend> *)
  let make_all ~g desc =
    Lwd_seq.map
      (function
        | Check (v, l, c) ->
            let n = Hashtbl.hash v in
            let name = name ~g ~n t.name in
            let elt, value = make_single ~persist name v l c in
            (elt, value))
      desc
  in
  let all = make_all ~g:0 t.desc in
  let elts = Lwd_seq.map (fun (elt, _) -> elt) all in
  let value =
    Lwd_seq.fold_monoid
      (fun (_, v) -> Lwd_seq.element (Lwd.get v))
      Lwd_seq.monoid all
    |> Lwd_seq.lift |> Lwd_seq.filter_map Fun.id
  in
  { field = Elwd.div ?at [ `S (Lwd_seq.lift elts) ]; value }

(* let rec make_all ~g ~n (acc_elt, acc_value) desc =
     match desc with
     | Check (v, l, c) :: tl ->
         let elt, value = make_check ~g ~n v l c in
         let acc_elt = Lwd_seq.concat acc_elt @@ Lwd_seq.element elt in
         let acc_value =
           Lwd.map2 acc_value value ~f:(fun acc -> function
             | None -> acc | Some v -> v :: acc)
         in
         make_all ~g ~n:(n + 1) (acc_elt, acc_value) tl
     | _ -> (Lwd.pure acc_elt, acc_value)
   in
   let elts, value = make_all ~g:0 ~n:0 (Lwd_seq.empty, Lwd.pure []) t.desc in
   { field = Elwd.div [ `S (Lwd_seq.lift elts) ]; value } *)
