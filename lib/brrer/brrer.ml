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

    let draggable s = v Name.draggable s
  end
end

module Brr_webworkers = Brr_webworkers
module Brr_webmidi = Brr_webmidi
module Brr_webgpu = Brr_webgpu
module Brr_webcrypto = Brr_webcrypto
module Brr_webaudio = Brr_webaudio
module Brr_io = Brrer_io
module Brr_canvas = Brr_canvas
module Fut = Fut
