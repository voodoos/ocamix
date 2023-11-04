type lang' = Fr | En

module Config : Brr_lwd_ui.Translated.Config with type lang = lang' = struct
  type lang = lang'

  let strings =
    [
      ((Fr, "click"), "Cliquez sur le bouton !");
      ((En, "click"), "Click on the Button !");
    ]

  let active = Lwd.var Fr

  let get lang string =
    List.assoc_opt (lang, string) strings |> Option.value ~default:string
end

let set, _s = Brr_lwd_ui.Translated.make (module Config)
let _s s f = Lwd.map (_s s) ~f
