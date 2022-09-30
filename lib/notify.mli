(** Notify is used to tell people about problems

    In general, the logger records the warnings and notifcations. Then they are
    packaged up here and emitted to the appropriate places. We wait to package
    up notifications because sometimes we may be unsure where to send a
    notification until we have both GitHub and Forecast data.

 *)


type target =
  | NoTarget
  | GitHub
  | Slack
  | All       (** *)
(** Where notifications should be posted. The default is NoTaget. *)

val dump_log : unit -> unit
