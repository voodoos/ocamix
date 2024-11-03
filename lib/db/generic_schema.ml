module Artist = struct
  type t = { name : string } [@@deriving yojson]
end

module Genre = struct
  type t = { name : string } [@@deriving yojson]
end

type source = Jellyfin of { id : string } [@@deriving yojson]
type 'a with_source = { source_info : source; item : 'a } [@@deriving yojson]
