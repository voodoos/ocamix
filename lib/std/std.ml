include ContainersLabels

module String = struct
  include String
  module Set = Set.Make (String)
end
