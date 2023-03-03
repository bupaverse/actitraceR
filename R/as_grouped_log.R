#' Group log by cluster
#'
#' Group log by its respective trace cluster as calculated with `cluster_traces()`. It is possible to access
#' information on traces per cluster with `attr()`.
#'
#' @param x An object of type `trace_clustering` as returned by `cluster_traces()`.
#' @param ... Additional parameters
#'
#' @return A grouped log on cluster variable
#'
#' @import dplyr
#'
#' @export
as_grouped_log <- function(x, ...) {
  UseMethod("as_grouped_log")
}

#' @export
as_grouped_log.trace_clustering <- function(x, ...) {
  cluster <- NULL
  # add cluster column
  # group on cluster
  # return grouped_eventlog

  for (i in 1:length(x)) {x[[i]] <- x[[i]] %>% mutate(cluster = i)}

  output <- bind_rows(x) %>% group_by(cluster)
  attr(output, "traces") <- attr(x, "traces")
  return(output)
}



