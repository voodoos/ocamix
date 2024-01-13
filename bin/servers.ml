open Import

type server = { connexion : DS.connexion; status : Db.Sync.report Lwd.var }
type t = (string * server) list
