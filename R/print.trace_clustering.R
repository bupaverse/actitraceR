#' Print clustering
#'
#' @param x Clustering
#' @param ... Not used.
#'
#' @return Prints the result of `cluster` function.
#' @export


print.trace_clustering <- function(x, ...) {

  cat("\n")
  cl_sizes <- purrr::map(x, ~n_cases(.x)) %>% unlist %>% paste(collapse = ", ")
  cli::cli_alert_success(paste0("Trace clustering with ", length(x), " clusters of sizes ", cl_sizes, "\n "))
  cat("\n")
  cli::cli_alert_info("Before clustering - fitness & precision: ")
  print(attr(x, "total_metrics"))
  cat("\n")
  cli::cli_alert_info("After clustering - fitness & precision: ")
  print(attr(x, "cluster_metrics"))

}
