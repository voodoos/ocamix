include Std
module DS = Data_source.Jellyfin

module Utils = struct
  let with_timing ?(name = "") f =
    let t = Brr.Performance.now_ms Brr.G.performance in
    let res = f () in
    Brr.Console.log
      [ name; "took"; Brr.Performance.now_ms Brr.G.performance -. t ];
    res
end
