


#' Select a cluster
#'
#' @param trace_clustering  Result of `cluster_traces`.
#' @param cluster_id cluster to select
#'
#' @return Log of selected cluster
#' @export
#'
#'
select_cluster <- function(trace_clustering, cluster_id) {

  if(cluster_id > length(trace_clustering))
    cli::cli_abort(glue::glue("There are only {length(trace_clustering)} clusters."))
  else {
    return(trace_clustering[[cluster_id]])
  }
}


