type what = Panic | Error | Warn | Info
type destination = Console

val notify : destination -> what -> string -> unit
