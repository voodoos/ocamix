open! Import

type layout = Main | Kiosk
type cover_type = Front | Back

let active_layout = Lwd.var Main
let kiosk_cover = Lwd.var Front
