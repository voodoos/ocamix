include Std

module Utils = struct
  let with_timing f =
    let t = Brr.Performance.now_ms Brr.G.performance in
    let res = f () in
    Brr.Console.log [ "Took"; Brr.Performance.now_ms Brr.G.performance -. t ];
    res
end
