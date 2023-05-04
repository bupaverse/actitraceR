#' Cluster an event log based on its traces.
#'
#' Implementation of the active trace clustering (ActiTraC) algorithm.
#'
#' @param log [log]: An object of type [`log`].
#' @param number_of_clusters [num]: A number of clusters.
#' @param target_fitness [num]: A target value to compare with the log fitness.
#' @param minimal_cluster_size [num]: A minimal cluster size. Defined as a percentage.
#' @param window_size [num]: A window size of the most frequent traces, i.e. traces with the highest absolute frequency. If `1`, all of the remaining traces are evaluated in the distance based method. If `0`, only one trace with the highest absolute frequency is chosen (frequency-based method).
#' @param cluster_remaining_traces [lgl] (default `FALSE`): Whether to cluster the remaining traces together. If `FALSE` (default), the remaining traces are assigned to that cluster, with which it has the best fitness. Otherwise, clusters the remaining traces together in a new additional cluster.
#' @param discover_algorithm (default `pm4py::discover_inductive`): A process discovery algorithm, returning a Petri Net
#' @param distance_metric (default "stringdist"): Distance calculation method.
#' - stringdist: `stringdist()` of a `stringdist` package is used.
#' - mra: Maximal repeat alphabet
#' @param ... Additional parameters for discovery algorithm
#' @return [list] A `list` object containing event logs filtered on trace id's, as a result of clustering algorithm.
#' @seealso This function is an implementation of the active trace clustering (ActiTraC) algorithm: J. De Weerdt, S. vanden Broucke, J. Vanthienen and B. Baesens, "Active Trace Clustering for Improved Process Discovery," in IEEE Transactions on Knowledge and Data Engineering, vol. 25, no. 12, pp. 2708-2720, Dec. 2013, doi: 10.1109/TKDE.2013.64.
#'
#' @importFrom stringdist stringdist
#' @importFrom stringr str_detect
#' @importFrom purrr map map2 transpose
#' @importFrom pm4py discover_inductive fitness_alignments precision_alignments
#'
#' @export
cluster_traces <- function(log,
                           number_of_clusters = 3,
                           target_fitness = 1,
                           minimal_cluster_size = 0.6,
                           window_size = 0.25,
                           cluster_remaining_traces = F,
                           discover_algorithm = pm4py::discover_inductive,
                           distance_metric = stringdist(method = "osa"),
                           ...) {

  num_clusters <- number_of_clusters
  mcs <- minimal_cluster_size
  activity_id <- lifecycle_id <- trace_id <- absolute_frequency <- NULL

  # Prep
  log_original <- log
  log[[activity_id(log)]] <- log[[activity_id(log)]] %>% as.character()
  log <- log[log[[lifecycle_id(log)]] == "complete", ] # or distinct()
  dict <- get_dict(log)


  CS <- list() # set of clusters
  tokenized_traces <- get_tokenized_traces(log = log, dict) # [tbl_df] set of all dpi'

  options(cli.progress_show_after = 0)
  cli::cli_progress_bar("Clustering traces", total = nrow(tokenized_traces) + 1, )
  cli::cli_progress_update(inc = 1)

  distance_arg <- enexpr(distance_metric)

  if(str_detect(deparse(distance_arg[[1]]), "stringdist")) {
    equivalence_classes <- NULL
  } else if(str_detect(deparse(distance_arg[[2]]), "mra")) {
    equivalence_classes <- get_equivalence_classes(tokenized_traces, dict)
  }
  R <- tokenized_traces # [tbl_df] set of remaining dpi's

  # line 4: start of the algorithm ------------------------------------------
  while (length(CS) < num_clusters && nrow(R) > 0) {
    C <- data.frame() # set of dpi's in current cluster
    # I <- c() # [chr] set of ignored dpi's (or trace_id's)
    I <- data.frame()

    # line 7-8: Phase 1: Selection (repeat) -----------------------------------
    # message("===================================")
    # message("Phase 1: selection")
    # message("Cluster ", length(CS) + 1, " is being formed")

    while (nrow(R) > 0 && !identical(sort(R$trace_id), sort(I$trace_id))) {
      W <- get_window_traces(R = R, I = I, window_size = window_size)

      # line 10: if current cluster C empty or only one dpi candidate -----------
      if (nrow(C) == 0 || nrow(W) == 1) {
        cur_dpi <- W[1, ]
        # message("--------------------------------")
        # message("trace_id ", cur_dpi$trace_id, " is being evaluated")
      }

      # both conditions are false
      # Thus, cluster is not empty AND there are more than one candidates
      else {
      #   message("--------------------------------")
      #   message("cluster ", length(CS) + 1, " is not empty and there are more than one dpi candidates")

        cur_dpi <- shortest_dist_dpi(dict = dict, C = C, W = W, distance_metric = distance_arg, equivalence_classes)

        # message("trace_id ", cur_dpi$trace_id, " has the lowest avg. distance")
      }

      # line 15: Inductive Miner (IM) -------------------------------------------
      # Apply IM on groups of event logs with dpi's from C and cur_dpi
      C_curdpi <- rbind(C, cur_dpi)
      trace_ids <- C_curdpi %>% pull(trace_id)
      discovered <- discover_pm(log, trace_ids, algo = discover_algorithm, ...)
      # GL <- log %>% edeaR::filter_trace(trace_ids = trace_ids)
      # PM <- pm4py::discovery_inductive(eventlog = GL, convert = T)


      # line 16: test conditions on fitness and cluster size --------------------
      # is log fitness >= target fitness?
      reticulate::py_capture_output({alignment <- pm4py::fitness_alignments(discovered$GL, marked_petrinet = discovered$PM)})
      # message("... with log_fitness = ", alignment$log_fitness)

      # size_dpi_C: curr. cluster size (absolute_frequency) WITHOUT candidate cur_dpi
      # size_dpi_R: absolute frequency of remaining traces INCLUDING candidate cur_dpi


      size_dpi_C <- tokenized_traces %>% filter(trace_id %in% C$trace_id) %>% pull(absolute_frequency) %>% sum() # returns 0 if C is empty
      size_dpi_R <- sum(R$absolute_frequency)

      if (alignment$log_fitness >= target_fitness) {
        # message("... + C, - R")
        C <- C_curdpi
        R <- R %>% filter(trace_id != cur_dpi$trace_id)
        cli::cli_progress_update(inc = 1)

      } else if (size_dpi_C >= mcs * size_dpi_R) { # size_dpi_C > 0 ==> C is not empty
        # cat("", sep = "\n")
        # message("--- Phase 2: look ahead ---")
        # message("evaluating remaining dpi\'s from R:")
        trace_ids <- C %>% pull(trace_id)
        discovered <- discover_pm(log, trace_ids, algo = discover_algorithm, ...)


        # line 22: Look ahead -----------------------------------------------------
        for (i in R$trace_id) { # i is a trace_id of dpi from R
          dpi <- R %>% filter(trace_id == i)
          GL_i <- log %>% edeaR::filter_trace(trace_ids = i) # fitness(i, PM)
          reticulate::py_capture_output({alignment <- pm4py::fitness_alignments(GL_i, marked_petrinet = discovered$PM)})

          # message("trace_id ", dpi$trace_id, " w/ fitness ", alignment$log_fitness)

          if (alignment$log_fitness >= target_fitness) {
            # message("... + C, - R")
            C <- rbind(C, dpi)
            R <- R %>% filter(trace_id != dpi$trace_id)
            cli::cli_progress_update(inc = 1)

          }
        }
        break
      }
      else {
        # message("trace_id ",  cur_dpi$trace_id)
        # message("... + I (ignored)")

        I <- I %>% rbind(cur_dpi)
      }
    }
    CS <- CS %>% append(list(C))
  }


  # line 37: Phase 3: residual trace resolution -----------------------------
  # cat("", sep = "\n")
  # message("--- Phase 3: residual trace resolution ---")

  if (nrow(R) > 0) { # if R is not empty

    # cluster all remaining traces into one cluster
    if (cluster_remaining_traces) {
      C <- R
      CS <- CS %>% append(list(R))
      cli::cli_progress_done()


      # message("remaining traces have been clustered together (cluster ", length(CS),")")
    }

    # distribute all remaining traces based on the best fitness with each cluster
    else {
      # message("clustering remaining traces based on their fitness with the respective cluster:")

      trace_ids_list <- CS %>% map(~pull(., trace_id))
      tmpPMs <- purrr::map(trace_ids_list, ~discover_pm(log, trace_ids = .x, algo = discover_algorithm, ...)) %>% purrr::map(~.x[names(.x) == "PM"])
      trace_ids <- R %>% pull(trace_id)

      # for each trace in R:
      #   calculate fitness_alignment for each cluster
      #   and get log_fitness
      for (i in 1:length(trace_ids)) {
        a <- map(tmpPMs, ~calc_fitness(.x$PM, log = log, trace_id = trace_ids[i]))
        a <- a %>% transpose

        better_fit_cluster <- a$log_fitness %>% which.max()
        dpi <- R %>% filter(trace_id == trace_ids[i])
        CS[[better_fit_cluster]] <- CS[[better_fit_cluster]] %>% rbind(dpi)
        cli::cli_progress_update(inc = 1)
        # message("trace_id ", trace_ids[i], " ... is added to the cluster ", better_fit_cluster)
      }
      cli::cli_progress_done()
    }
  }

  # RETURN ORIGINAL LOG including both lifecycles (not only "complete")
  trace_ids <- CS %>% purrr::map(~pull(., trace_id))
  logs_list <- trace_ids %>% purrr::map(~filter_trace(log = log, trace_ids = .x)) # thus, using log


  cli::cli_alert_info("Evaluating clusters")

  # assign attributes of list of logs
  # fitness
  PM_list <- purrr::map(trace_ids, ~discover_pm(log, .x, algo = discover_algorithm, ...)$PM)
  # PM_list <- logs_list %>% purrr::map(~discover_inductive(.x, convert = T))
  reticulate::py_capture_output({fitness_list <- map2(logs_list, PM_list, ~pm4py::fitness_alignments(.x, marked_petrinet = .y, convert = T)) %>%
    purrr::map(as.data.frame) %>%
    bind_rows()})

  # precision
  reticulate::py_capture_output({precision_list <- map2_dbl(logs_list, PM_list, ~pm4py::precision_alignments(.x, marked_petrinet = .y, convert = T)) %>%
    tibble(precision = .) %>%
    mutate(cluster = 1:n())})
  cluster_metrics <- cbind(fitness_list, precision_list)

  # total
  PM <- discover_inductive(log, convert = T)
  reticulate::py_capture_output({fitness_total <- fitness_alignments(log, marked_petrinet = PM) %>%
    as.data.frame()})
  reticulate::py_capture_output({precision <- precision_alignments( log, marked_petrinet = PM)})
  total_metrics <- cbind(fitness_total, precision)


  # assign attributes to ORIGINAL LOG
  original_logs <- trace_ids %>% purrr::map(~filter_trace(log = log_original, trace_ids = .x))

  attr(original_logs, "traces") <- CS
  attr(original_logs, "cluster_metrics") <- cluster_metrics
  attr(original_logs, "total_metrics") <- total_metrics
  # attr(logs_list, "fitness_total") <- fitness_total
  # attr(logs_list, "total_precision") <- total_precision

  # assign class
  class(original_logs) <- c("trace_clustering", class(original_logs))

  return(original_logs)
}

