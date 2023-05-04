#' Augment original log with cluster ids
#'
#' @param trace_clustering Result of `cluster_traces`.
#'
#' @return Log with added column `cluster`
#' @export
#'
#
augment_cluster <- function(trace_clustering) {

  tibble(cluster = 1:length(trace_clustering), data = trace_clustering) %>%
    unnest('data') %>%
    re_map(mapping(trace_clustering[[1]]))
}
