open! Import
open Common
open Brrer
open! Brr
open! Brr_lwd

let logger = Logger.for_section "virtual grid"

let make (layout : _ Layout.fixed_grid) render (data_source : _ Data_source.t) =
  let _state = new_state layout in
  ignore (render, data_source);
  ()
