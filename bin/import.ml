include Std
module DS = Data_source.Jellyfin
module Elwd = Brr_lwd.Elwd
module Ui_utils = Brr_lwd_ui.Utils
module View = Db.View
module Attrs = Brr_lwd_ui.Attrs
module IDB = Brr_io.Indexed_db

module String = struct
  include String
  module MMap = CCMultiMap.Make (String)
  module Items_MultiMap = MMap (Db.Stores.Items)
end

module Utils = struct
  let with_timing ?(name = "") f =
    let t = Brr.Performance.now_ms Brr.G.performance in
    let res = f () in
    Brr.Console.log
      [ name; "took"; Brr.Performance.now_ms Brr.G.performance -. t ];
    res
end
