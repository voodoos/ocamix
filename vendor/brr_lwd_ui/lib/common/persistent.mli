val local_storage : Brr_io.Storage.t

val var : key:string -> 'a -> 'a Lwd.var
(** [var ~key v] returns a new [Lwd.var].

    If [key] exists in the local_storage, the stored value is used as initial
    value instead of [v]. When this var's value is changed using the [Lwd.set]
    function its new value is persisted into the local_storage at the key [key].

    A uniqueness check on [key] is performed at runtime. *)

val var_f : key:string -> (unit -> 'a) -> 'a Lwd.var
(** Same as [var] but uses a callback to initialize the variable when needed. *)

val var_fut :
  key:string -> (unit -> ('a, 'b) Fut.result) -> ('a Lwd.var, 'b) Fut.result
(** Same but stores a Future's result in the variable. *)
