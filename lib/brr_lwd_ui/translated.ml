module type Config = sig
  type lang

  (* val default : lang *)
  val active : lang Lwd.var
  val get : lang -> string -> string
end

let make (type a) (module C : Config with type lang = a) =
  let get string =
    Lwd.map (Lwd.get C.active) ~f:(fun lang -> Jstr.v @@ C.get lang string)
  in
  let set (lang : a) = Lwd.set C.active lang in
  (set, get)
