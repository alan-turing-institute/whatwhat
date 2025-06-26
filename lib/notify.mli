(** Notify is used to tell people about problems

    In general, the logger records the warnings and notifcations. Then they are
    packaged up here and emitted to the appropriate places. We wait to package
    up notifications because sometimes we may be unsure where to send a
    notification until we have both GitHub and Forecast data.

    {{!page-reporting} An explanations of what gets reported is here} *)

module IntMap : module type of Map.Make (Int)

(** Where notifications should be posted. The default is NoTarget. *)
type notify_target =
  | NoTarget
  | Github
  | Slack
  | All

val post_github_comment_async : int -> string -> unit Lwt.t

(** Post all logged events to GitHub. *)
val post_github
  :  verbose:int
  -> restrict_codes:Log.code_spec
  -> restrict_issues:int list option
  -> unit
