open! Std

(* Generalities:
   - Sort names should be part of the values, not the keys. They will be used as
     indexes' keys for sorting.
   - Names should be part of the keys so that they can be used for filtering. *)

(* Indexeddb keys can be: string, date, float, a binary blob, and array. For
   arrays, the key can range from an empty value to infinity. And you can include
   an array within an array. *)

module Id = struct
  type t = Jellyfin of string [@@deriving yojson]

  let equal t t' =
    match (t, t') with Jellyfin id, Jellyfin id' -> String.equal id id'
end

module Collection = struct
  type t = { id : Id.t; name : string } [@@deriving yojson]
end

module Artist = struct
  type t = { name : string } [@@deriving yojson]
end

module Genre = struct
  type t = { name : string; canon : string } [@@deriving yojson]
end

module Album = struct
  type t = {
    (* name : string; *)
    id : Id.t;
    sort_name : string;
    genres : int list;
  }
  [@@deriving yojson]

  module Key = struct
    type t = { name : string; genres : int list } [@@deriving yojson]
  end
end

module Track = struct
  type t = {
    (* name : string; *)
    id : Id.t;
    album_id : Id.t option;
    sort_name : string;
    genres : int list;
    server_id : Id.t;
        (* TODO this should not be here track -> collection -> server*)
  }
  [@@deriving yojson]

  module Key = struct
    type t = { name : string; genres : int list; collections : int list }
    [@@deriving yojson]
  end
end
