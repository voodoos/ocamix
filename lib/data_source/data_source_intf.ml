open! Brr

module type S = sig
  type t
  type credentials

  val connect : credentials -> t
end
