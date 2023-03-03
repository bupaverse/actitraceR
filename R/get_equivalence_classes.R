#' Get equivalence classes from a set of traces
#'
#' Get equivalence classes from a set of traces based described in ...
#'
#' @param tokenized_traces [data.frame] A set of unique traces containing some additional metrics, a trace list and a tokenized trace list (tokens) as returned by `get_tokenized_traces()`.
#' @param dict A dictionary as returned by `get_dict()`.
#' @return A set of equivalence classes.
#'
#' @importFrom purrr map
#' @importFrom stringr str_count
#'
#' @export
get_equivalence_classes <- function(tokenized_traces, dict) {

  activities <- 1:length(dict) %>% paste0("[",., "]")

  tokenized_traces$tokens %>% map(paste, collapse = "") -> traces_strings

  max_length <- max(str_count(traces_strings, "\\["))

  full_log_trace <- traces_strings %>% paste(collapse = "---")

  create_candidates(max_length = 6, full_log_trace, activities = activities) -> candidates

  get_MRA(candidates, full_log_trace, activities)
}



# mra <- function(a, b, equivalence_classes) {
#
#   tibble(tokens = c(a,b)) -> output
#
#   for(i in 1:length(equivalence_classes)) {
#
#     feature_name <- paste0("EC_",str_remove_all(str_replace_all(names(equivalence_classes)[[i]], fixed("]["),"_"), "\\[|\\]"))
#
#     output[feature_name] <- 0
#
#     for(j in 1:length(equivalence_classes[[i]])) {
#       output[feature_name] <- output[feature_name] + str_count(output$tokens, fixed(equivalence_classes[[i]][j]))
#     }
#   }
#   output %>%
#     select(starts_with("EC")) %>%
#     dist(upper = T, diag = T) %>%
#     as.matrix %>%
#     .[1:length(a),-1:-length(a)] %>%
#     return()
# }
# GL_all


