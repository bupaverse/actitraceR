print.trace_clustering <- function(x, ...) {
  purrr::map(x, ~print(head(.x)))

  cat("\n")
  cl_sizes <- purrr::map(x, ~n_cases(.x)) %>% unlist %>% paste(collapse = ", ")
  cli::cli_alert_info(paste0("\n", "Active trace clustering with ", length(x), " clusters of sizes ", cl_sizes, "\n "))

  cat("\n", "Fitness: ", sep = "\n")
  print(attr(x, "fitness"))

  cat("\n", "Precision: ", sep = "\n")
  print(attr(x, "precision"))

  cat("\n", "Full log fitness & precision: ", sep = "\n")
  print(attr(x, "total"))
}
