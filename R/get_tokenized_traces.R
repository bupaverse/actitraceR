#' Get tokenized set of traces
#'
#' Get a set of unique traces containing some additional metrics, a trace list and a tokenized trace list (tokens).
#'
#' @param log [`log`] An object of type [`log`].
#' @param dict [list] A dictionary as returned by `get_dict()`.
#' @return [data.frame] A collection of unique traces with additional information.
#'
#' @export
get_tokenized_traces <- function(log, dict) {
  output <- log %>% bupaR::case_list(.keep_trace_list = T) %>%
    group_by(trace_id, trace, trace_list) %>%
    summarize(absolute_frequency = n(), .groups = "drop_last") %>%
    mutate(relative_frequency = absolute_frequency/sum(absolute_frequency)) %>%
    arrange(desc(absolute_frequency)) %>%
    select(trace_id, trace, absolute_frequency, relative_frequency, trace_list) %>%
    mutate(tokens = map(trace_list, to_token, dict))

  cli::cli_alert_info(paste("log consisting of", nrow(output), "traces"))
  return(output)
}

