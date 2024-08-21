include ContainersLabels
module Brr = Brrer.Brr

(** [tee f x] applies [f] to [x] and returns [x] *)
let tee f x =
  let () = f x in
  x

module String = struct
  include String
  module Set = Set.Make (String)
end

module Lwd = struct
  include Lwd
end
