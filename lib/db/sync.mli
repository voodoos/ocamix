(** This module provides utilities to synchronize the local (Indexed_db)
    database with the remote source.

    For each of the source's music views we fetch every item it contains through
    a flat, unsorted, paginated query, several pages at a time. This is done in
    three phases -- artists, albums, then tracks -- because a track can only be
    linked to its album once that album is in the database.

    Whether a view needs to be synchronized at all is decided by comparing its
    number of tracks in the source with the number we have locally. Items
    already present are rejected by the stores' unique indexes, so a
    re-synchronization converges instead of duplicating.

    TODO: we only keep track of added / removed items, but items details can
    change (like changes in metadata, genres, etc)

    TODO: items removed from the source are never removed locally. *)

type status =
  | Unknown
  | Syncing
  | In_sync
  | Inconsistent
  | New_items of {
      first_missing_key : int;
      first_unfetched_key : int;
      last_source_item_key : int;
    }
  | Partial_fetch of { first_unfetched_key : int; last_source_item_key : int }

val log_status : status -> unit

type count = {
  mutable artists : int;
  mutable albums : int;
  mutable tracks : int;
}
[@@deriving jsont]

type progress = { total : count; processed : count; jobs : int }

type report = { status : status; sync_progress : progress option }
[@@deriving jsont]

val initial_report : report
val pp_report : Format.formatter -> report -> unit

val check_and_sync :
  ?report:(report -> unit) ->
  source:Data_source.Jellyfin.connexion ->
  Brrer.Brr_io.Indexed_db.Database.t ->
  (unit, Jv.Error.t) Fut.result
(** [check_and_sync] cheks that the current db is consistent with the source and
    makes appropriate updates if necessary *)
