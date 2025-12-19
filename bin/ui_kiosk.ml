open Import
open Brr

let big_cover =
  let display_none =
    Lwd.map (Lwd.get App_state.active_layout) ~f:(function
      | Main -> At.class' (Jstr.v "display-none")
      | Kiosk -> At.void)
  in
  let src_front =
    Lwd.map (Lwd.get Player.now_playing) ~f:(function
      | None -> Fut.return (Some "track.png")
      | Some
          {
            item =
              Db.Generic_schema.Track.(
                _, { server_id = Jellyfin server_id; _ }, album);
            _;
          } ->
          let servers = Lwd_seq.to_list (Lwd.peek Servers.connexions) in
          let connexion : DS.connexion = List.assq server_id servers in
          Fut.return
            (Player.get_album_cover_link_opt ~base_url:connexion.base_url
               ~size:1024 ~cover_type:Front album))
  in
  let src_back =
    Lwd.map2 (Lwd.get Player.now_playing) (Lwd.get App_state.kiosk_cover)
      ~f:(fun np back ->
        Fut.return
        @@
        match (np, back) with
        | ( Some
              {
                item =
                  Db.Generic_schema.Track.(
                    _, { server_id = Jellyfin server_id; _ }, album);
                _;
              },
            Back ) ->
            let servers = Lwd_seq.to_list (Lwd.peek Servers.connexions) in
            let connexion : DS.connexion = List.assq server_id servers in
            Player.get_album_cover_link_opt ~base_url:connexion.base_url
              ~size:1024 ~cover_type:Back album
        | _, _ -> None)
  in
  let front_image_preloader = Brr_lwd_ui.Img.preloader src_front in
  let back_image_preloader = Brr_lwd_ui.Img.preloader src_back in
  let front_image_status = Brr_lwd_ui.Img.status front_image_preloader in
  let back_image_status = Brr_lwd_ui.Img.status back_image_preloader in
  let background =
    Lwd.map (Brr_lwd_ui.Img.object_url front_image_preloader) ~f:(fun src ->
        let src = Option.value ~default:"track.png" src in
        At.style (Jstr.v (Printf.sprintf "background-image: url(%s)" src)))
  in
  let backdrop_blur elts =
    let filter_style =
      At.style
        (Jstr.v
           "position: relative; width: 100%; height: 100%; backdrop-filter: \
            blur(16px);")
    in
    Elwd.div
      ~at:[ `R background ]
      [ `R (Elwd.div ~at:[ `P filter_style ] elts) ]
  in
  let cover =
    let front_img =
      Brr_lwd_ui.Img.render_img ~revoke_on_load:true
        ~at:[ `P (At.class' (Jstr.v "face front")) ]
        front_image_preloader
    in
    let back_img =
      Brr_lwd_ui.Img.render_img ~revoke_on_load:true
        ~at:[ `P (At.class' (Jstr.v "face back")) ]
        back_image_preloader
    in
    let flipped =
      (* We need to keep track of previous state to debounce flippling *)
      let is_flipped = ref false in
      Lwd.map2 (Lwd.get App_state.kiosk_cover) back_image_status
        ~f:(fun view status ->
          match (view, status) with
          | Back, Ok ->
              is_flipped := true;
              At.class' (Jstr.v "flip")
          | Back, Loading when !is_flipped -> At.class' (Jstr.v "flip")
          | _, _ ->
              is_flipped := false;
              At.void)
    in
    let on_click =
      Elwd.handler Ev.click (fun e ->
          Ev.prevent_default e;
          (match Lwd.peek App_state.kiosk_cover with
            | Front -> Back
            | Back -> Front)
          |> Lwd.set App_state.kiosk_cover)
    in
    Elwd.div
      ~ev:[ `P on_click ]
      ~at:[ `P (At.class' (Jstr.v "two-face-cover")); `R flipped ]
      [ `R back_img; `R front_img ]
  in
  let loading =
    Lwd.map2 front_image_status back_image_status ~f:(fun s1 s2 ->
        match (s1, s2) with
        | Loading, _ | _, Loading -> At.class' (Jstr.v "loading")
        | _ -> At.void)
  in
  let no_back_image =
    Lwd.map2 (Lwd.get App_state.kiosk_cover) back_image_status
      ~f:(fun v status ->
        match (v, status) with
        | Back, Error -> At.class' (Jstr.v "no-back")
        | _ -> At.void)
  in
  let more_classes =
    (* TODO It looks like a Brr_lwd issue that we need a seq to have multiple
       reactive classes.*)
    Lwd.pure @@ Lwd_seq.of_list [ display_none; no_back_image; loading ]
  in
  let at =
    [ `P (At.class' (Jstr.v "big-cover")); `S (Lwd_seq.lift more_classes) ]
  in
  Elwd.div ~at [ `R (backdrop_blur [ `R cover ]) ]
