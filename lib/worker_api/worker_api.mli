module type Queries = sig
  type 'a query
  (** Use a GADT to describe queries that have different parameters and return
      value. For example:
      {[ type 'a query =
           | Next_int : int -> int query
           | Previous_letters : char -> char list query ]}
  *)
end

module Make : functor (Q : Queries) -> sig
  type error = [ `Jv of Jv.Error.t | `Msg of string ]
  type 'a query = 'a Q.query

  (** The [Client] functor should be use to open a connexion to the worker and
      provides a standardized interface for querying it. *)
  module Start_client : functor
    (_ : sig
       val url : string
       (** The url were the implementation of the worker can be loaded from. *)
     end)
    -> sig
    val worker : Brr_webworkers.Worker.t
    val query : 'a query -> ('a, error) Fut.result
  end

  module type Worker_impl = functor () -> sig
    val on_query : 'a query -> ('a, error) Fut.result
    (** [on_query q] should return the result of the query q *)
  end

  (** Use [Make_worker] to generate the body of the worker. The generative
      functor given in parameter contian the program t be executed, and should
      provide an [on_query] function to answer all messages defined by the API.
  *)
  module Make_worker : functor (_ : Worker_impl) -> sig end
end
