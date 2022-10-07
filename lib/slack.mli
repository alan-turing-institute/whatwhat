(** Post a [msg] to Slack.

    If the HTTP POST request fails, raise a [HttpError]. If it passes, but the Slack API
    returns a response that indicates an error, raise a [SlackAPIError]. If the Slack API
    response indicates success with a warning, print the warning to [stdout].

    Return [()].*)
val post : string -> unit
