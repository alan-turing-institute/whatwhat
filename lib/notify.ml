(** Pings people about problems on GitHub (and perhaps Slack, eventually). *)

module IntMap = Map.Make (Int)

type notify_target =
  | NoTarget
  | Github
  | Slack
  | All

let format_metadata_report_github (events : Log.event list) : string =
  let errors = List.filter Log.isError events in
  let warnings = List.filter Log.isWarning events in
  let error_msgs = List.map (fun ev -> ev.Log.message) errors in
  let warning_msgs = List.map (fun ev -> ev.Log.message) warnings in
  let n_errors = List.length errors in
  let n_warnings = List.length warnings in
  let buf = Buffer.create 128 in
  Buffer.add_string buf "Beep boop! I'm a bot.\n\n";
  if n_errors > 0
  then (
    Buffer.add_string
      buf
      "I was unable to read the [YAML metadata \
       block](https://github.com/alan-turing-institute/Hut23/blob/master/.github/ISSUE_TEMPLATE/project.md) \
       at the top of this issue because of the following **error(s)**:\n\n\
       - ";
    Buffer.add_string buf (String.concat "\n- " error_msgs))
  else
    Buffer.add_string
      buf
      "I had trouble reading the [YAML metadata \
       block](https://github.com/alan-turing-institute/Hut23/blob/master/.github/ISSUE_TEMPLATE/project.md) \
       at the top of this issue because of the following **problem(s)**:\n\n\
       - ";
  if n_errors > 0 && n_warnings > 0
  then Buffer.add_string buf "\n\nIn addition, I had the following **problem**(s):\n\n- ";
  if n_warnings > 0 then Buffer.add_string buf (String.concat "\n- " warning_msgs);
  Buffer.contents buf
;;

let post_metadata_reports_github () =
  failwith "Implement this"
  (* let metadata_reports = *)
  (*   Log.get_the_log () *)
  (*   |> extract_metadata_events *)
  (*   |> IntMap.map format_metadata_report_github *)
  (*   |> IntMap.to_seq *)
  (* in *)
  (* Printf.printf *)
  (*   "Posting metadata reports to the following %d projects:\n" *)
  (*   (Seq.length metadata_reports); *)
  (* Seq.iter *)
  (*   (fun (nmbr, report) -> *)
  (*     Printf.printf "hut23-%d; " nmbr; *)
  (*     flush stdout; *)
  (*     ignore @@ GithubBot.github_post "Hut23" nmbr report; *)
  (*     Unix.sleep 2) *)
  (*   metadata_reports *)
(* ;; *)
