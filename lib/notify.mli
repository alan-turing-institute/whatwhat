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
  | All

val dump_the_log : unit -> unit
val dump_metadata_events : unit -> unit
val extract_metadata_events : Log.event Seq.t -> Log.event list IntMap.t

(** Print a more concise form of metadata reports. *)
val print_metadata_reports : bool -> unit

(** Format metadata reports in a suitable manner for posting to GitHub. *)
val format_metadata_report_github : Log.event list -> string

(** Post metadata reports as GitHub issue comments. *)
val post_metadata_reports_github : unit -> unit
