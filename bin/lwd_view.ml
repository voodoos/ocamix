open Import

type ordered = {
  request : View.req Lwd.t;
  item_count : int Lwd.t;
  start_offset : int Lwd.t;
  order : View.Order.t Lwd.t;
}

let to_view view =
  Ui_utils.map3 view.request view.start_offset view.item_count
    ~f:(fun request start_offset item_count ->
      { View.request; start_offset; item_count })
