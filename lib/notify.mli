(** Notify is used to tell people about problems

    In general, the logger records the warnings and notifcations. Then they are
    packaged up here and emitted to the appropriate places. We wait to package
    up notifications because sometimes we may be unsure where to send a
    notification until we have both GitHub and Forecast data.

    {{!page-reporting}An explanations of what gets reported is here} 
    
 *)

module IntMap : module type of Map.Make (Int)

(** Where notifications should be posted. The default is NoTarget. *)
type notify_target =
  | NoTarget
  | Github
  | Slack
  | All (** *)

val dump_the_log : unit -> unit
val dump_metadata_events : unit -> unit
val extract_metadata_events : Log.event Seq.t -> Log.event list IntMap.t
val format_metadata_report : Log.event list -> string

(** Write the reports that would be posted to Github issue comments to standard 
    out *)
val print_metadata_reports : unit -> unit

(** Post metadata reports as Github issue
    comments *)
val post_metadata_reports : unit -> unit
