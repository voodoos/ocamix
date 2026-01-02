open Import

type ordered = {
  request : View.req Lwd.t;
  item_count : int Lwd.t;
  start_offset : int Lwd.t;
  order : View.Order.t Lwd.t;
  keys :
    (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t option
    Lwd.t;
}

let to_view view =
  Ui_utils.map4 view.request view.start_offset view.item_count view.keys
    ~f:(fun request start_offset item_count keys ->
      ({ View.request; start_offset; item_count; duration = 0. }, keys))
