open! Import
open! Brrer
open! Brr
open! Brr_lwd

type 'a reactive_field = {
  field : Elwd.t Lwd.t;
  label : Elwd.t Lwd.t;
  value : 'a;
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
    Elwd.select ~at ~ev
      ~on_create:(fun e -> element := Some e)
      [ `S (Lwd_seq.lift options) ]
  in
  let () =
    Utils.tap ~f:(fun v ->
        Option.iter
          (fun select -> Jv.set (El.to_jv select) "value" (Jv.of_string v))
          !element)
    @@ Lwd.get value
  in
  { field; label; value }

let rec fuzzy_substring remaining s =
  match (s (), remaining) with
  | _, [] -> true
  | Seq.Nil, _ -> false
  | Cons (c, rest), c' :: tl ->
      if Char.equal (Char.lowercase_ascii c) (Char.lowercase_ascii c') then
        fuzzy_substring tl rest
      else fuzzy_substring remaining rest

let make_multiple ?(persist = false) ?(at = [])
    (options : ('a, bool) Field_checkboxes.group) =
  let open Field_checkboxes in
  let focused = Lwd.var false in
  let search_input = Field.text_input None in
  let filter =
    Lwd.map (Lwd.get search_input.value) ~f:(fun s ->
        let chars = String.to_list s in
        fun (Field_checkboxes.Check { name; _ }) ->
          fuzzy_substring chars (String.to_seq name))
  in
  let checkboxes = Field_checkboxes.make ~persist ~filter options in
  let select_all =
    let on_change =
      Lwd.map checkboxes.filtered ~f:(fun checkboxes ->
          Elwd.handler Ev.change @@ fun ev ->
          let t = Ev.target ev |> Ev.target_to_jv in
          let checked = Jv.get t "checked" in
          let f =
            if Jv.to_bool checked then fun var v -> Lwd.set var (Some v)
            else fun var _ -> Lwd.set var None
          in
          List.iter (Lwd_seq.to_list checkboxes)
            ~f:(fun { desc = Check { value; state; _ }; _ } -> f state value))
    in
    let all_filtered_values =
      Lwd_seq.fold
        ~map:(fun { desc = Check { state; _ }; _ } ->
          Lwd.map (Lwd.get state) ~f:(fun v -> Option.is_some v))
        checkboxes.filtered ~reduce:(Lwd.map2 ~f:( && ))
      |> Lwd.map ~f:(function None -> Lwd.pure false | Some b -> b)
      |> Lwd.join
    in
    let { element; desc = Check { state = select_all_var; _ } } =
      let ev = [ `R on_change ] in
      Field_checkboxes.make_single ~ev ~persist:false
        {
          value = ();
          id = "allselect-notuniquenamechangeme";
          name = "";
          label = (fun () -> []);
          state = false;
        }
    in
    let () =
      Utils.tap
        ~f:(function
          | true -> Lwd.set select_all_var (Some ())
          | false -> Lwd.set select_all_var None)
        ~initial_trigger:true all_filtered_values
    in
    element
  in
  let current_selection =
    let pills =
      let at = [ `P (At.class' (Jstr.v "lwdui-select-multiple-pill")) ] in
      Lwd_seq.map
        (fun (_, Field_checkboxes.Check { label; state; _ }) ->
          let unselect_button =
            let on_click =
              Elwd.handler Ev.click (fun _ -> Lwd.set state None)
            in
            Elwd.button ~ev:[ `P on_click ] [ `P (El.txt' "X") ]
          in
          List.concat [ label (); [ `R unselect_button ] ] |> Elwd.span ~at)
        checkboxes.value
    in
    Elwd.div
      ~at:[ `P (At.class' (Jstr.v "lwdui-select-multiple-selected")) ]
      [ `S (Lwd_seq.lift pills) ]
  in
  let choices_panel =
    let search_bar =
      let at = [ `P (At.class' (Jstr.v "lwdui-select-multiple-search-bar")) ] in
      Elwd.div ~at [ `R search_input.elt; `R select_all ]
    in
    let at =
      [
        `P (At.class' (Jstr.v "lwdui-select-multiple-choices"));
        `R
          (Lwd.map (Lwd.get focused) ~f:(fun b ->
               At.if' (not b) (At.class' (Jstr.v "display-none"))));
      ]
    in
    Elwd.div ~at [ `R search_bar; `R checkboxes.field ]
  in
  let on_focus_in = Elwd.handler Ev.focusin (fun _ -> Lwd.set focused true) in
  let on_focus_out =
    Elwd.handler Ev.focusout (fun _ -> Lwd.set focused false)
  in
  {
    Field_checkboxes.field =
      Elwd.button
        ~at:(`P (At.class' (Jstr.v "lwdui-select-multiple")) :: at)
        ~ev:[ `P on_focus_in; `P on_focus_out ]
        [ `R current_selection; `R choices_panel ];
    all = checkboxes.all;
    filtered = checkboxes.filtered;
    value = checkboxes.value;
  }
