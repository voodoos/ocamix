(**
    This module provide utilities to synchronize he local (Indexed_db) database
    with the remote source.

    The syncing mechanism makes the assumption that the source can provide a
    list of items ordered by the date of their addition to the source. Items
    should have a stable index in this reference query unless some of them have
    been deleted from the source.

    Fetching this list can take a lot of time for big collections, so we use a
    table with placeholders that are progressively populated with items ids.

    This list is used to check consistency and freshness of the database without
    massive queries:

    - If the the last fetched record has the same id as the corresponding item
      in the source, the database is consistent.
    - If the last fetched record coincides with the latest item of the source,
      then the database is fully synchronized.

    The database might become out-of-sync if new items have been added to the
    source. In that case new placeholders are added and fetched.

    The database might become inconsistent if items are removed from the source,
    thus changing all following items' index in the reference query.

    TODO: inconsistency is not handled yet: the simplest workaround would be to
          simply re-synchronize the complete database.

    TODO: we only keep track of added / removed items, but items details can
          change (like changes in metadata, genrs, etc)

*)

type progress = { remaining : int }

val check_and_sync :
  ?report:(progress -> unit) ->
  source:Data_source.Jellyfin.connexion ->
  Brrer.Brr_io.Indexed_db.Database.t ->
  (unit, Jv.Error.t) Fut.result
(** [check_and_sync] cheks that the current db is consistent with the source and
  makes appropriate updates if necessary *)
