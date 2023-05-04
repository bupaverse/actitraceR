
#' Group by trace clustering
#'
#' @param trace_clustering Result of `cluster_traces`.
#'
#' @return Log with added column `cluster`, and grouped on that column.
#' @export
#'
#' @importFrom tidyr unnest
#'
group_by_cluster <- function(trace_clustering) {

  cluster <- NULL

  tibble(cluster = 1:length(trace_clustering), data = trace_clustering) %>%
    unnest("data") %>%
    re_map(mapping(trace_clustering[[1]])) %>%
    group_by(cluster)
}
