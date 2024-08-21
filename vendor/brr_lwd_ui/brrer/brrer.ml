module Jv = Jv
module Jstr = Jstr

module Brr = struct
  include Brr

  module El = struct
    module S = Style
    include El
    module Style = S
  end

  module Ev = struct
    include Ev

    module Submit = struct
      type 'a event = 'a t
      type t = Jv.t
    end

    let submit = Type.void (Jstr.v "submit")
  end

  module At = struct
    include At

    module Name = struct
      include Name

      let pattern = Jstr.v "pattern"
    end

    let draggable s = v Name.draggable s
  end
end

module Brr_io = struct
  include Brr_io
  module Indexed_db = Indexed_db

  module Media = struct
    include Media
    module Session = Media_session
  end

  module Storage = struct
    include Storage

    module Manager = struct
      (* https://developer.mozilla.org/en-US/docs/Web/API/StorageManager *)
      type t = Jv.t

      include (Jv.Id : Jv.CONV with type t := t)

      let persist t = Jv.call t "persist" [||] |> Fut.of_promise ~ok:Jv.to_bool
    end

    let manager (n : Brr.Navigator.t) =
      Jv.get (Brr.Navigator.to_jv n) "storage" |> Manager.of_jv
  end
end

module Brr_webworkers = Brr_webworkers
module Brr_webmidi = Brr_webmidi
module Brr_webgpu = Brr_webgpu
module Brr_webcrypto = Brr_webcrypto
module Brr_webaudio = Brr_webaudio
module Brr_canvas = Brr_canvas
module Fut = Fut
module Intersection_observer = Intersection_observer
module Mutation_observer = Mutation_observer
module Resize_observer = Resize_observer
module Dom_rect_read_only = Dom_rect_read_only
