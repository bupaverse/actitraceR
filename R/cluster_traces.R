#' Cluster an event log based on its traces
#'
#' Implementation of the active trace clustering algorithm as described in J. De Weerdt, S. vanden Broucke, J. Vanthienen and B. Baesens, "Active Trace Clustering for Improved Process Discovery," in IEEE Transactions on Knowledge and Data Engineering, vol. 25, no. 12, pp. 2708-2720, Dec. 2013, doi: 10.1109/TKDE.2013.64.
#'
#' @param log [log]: An object of type [`log`].
#' @param num_clusters [num]: A number of clusters.
#' @param target_fitness [num]: A target value to compare with the log fitness.
#' @param mcs [num]: A minimum cluster size to test whether the cluster is already big enough.
#' @param window_size [num]: A window size of the most frequent traces, i.e. traces with the highest absolute frequency. If `1`, all of the remaining traces are evaluated in the distance based method. If `0`, only one trace with the highest absolute frequency is chosen (frequency-based method).
#' @param cluster_remaining_traces [lgl] (default `FALSE`): Whether to cluster the remaining traces together. If `FALSE` (default), the remaining traces are assigned to that cluster, with which it has the best fitness. Otherwise, clusters the remaining traces together in a new additional cluster.
#' @param algo (default `pm4py::discovery_inductive`): A discovery algorithm.
#' @param distance_metric (default "stringdist"): A distance calculation method. By default ("stringdist"), `stringdist()` of a `stringdist` package is used. The literature suggest "mra".
#' @return [list] A `list` object containing event logs filtered on trace id's, as a result of clustering algorithm.
#' @seealso J. De Weerdt, S. vanden Broucke, J. Vanthienen and B. Baesens, "Active Trace Clustering for Improved Process Discovery," in IEEE Transactions on Knowledge and Data Engineering, vol. 25, no. 12, pp. 2708-2720, Dec. 2013, doi: 10.1109/TKDE.2013.64.
#'
#' @export
cluster_traces <- function(log = eventdataR::patients,
                           num_clusters = 3,
                           target_fitness = 1,
                           mcs = 0.6,
                           window_size = 0.25,
                           cluster_remaining_traces = F,
                           algo = pm4py::discovery_inductive,
                           distance_metric = stringdist(method = "osa"),
                           ...) {

  # Prep
  log_original <- log
  log[[activity_id(log)]] <- log[[activity_id(log)]] %>% as.character()
  log <- log[log[[lifecycle_id(log)]] == "complete", ] # or distinct()
  dict <- get_dict(log)

  CS <- list() # set of clusters
  tokenized_traces <- get_tokenized_traces(log = log, dict) # [tbl_df] set of all dpi's

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
    message("===================================")
    message("Phase 1: selection")
    message("Cluster ", length(CS) + 1, " is being formed")

    while (nrow(R) > 0 && !identical(sort(R$trace_id), sort(I$trace_id))) {
      W <- get_window_traces(R = R, I = I, window_size = window_size)


      # line 10: if current cluster C empty or only one dpi candidate -----------
      if (nrow(C) == 0 || nrow(W) == 1) {
        cur_dpi <- W[1, ]
        message("--------------------------------")
        message("trace_id ", cur_dpi$trace_id, " is being evaluated")
      }

      # both conditions are false
      # Thus, cluster is not empty AND there are more than one candidates
      else {
        message("--------------------------------")
        message("cluster ", length(CS) + 1, " is not empty and there are more than one dpi candidates")

        cur_dpi <- shortest_dist_dpi(dict = dict, C = C, W = W, distance_metric = distance_arg, equivalence_classes)

        message("trace_id ", cur_dpi$trace_id, " has the lowest avg. distance")
      }


      # line 15: Inductive Miner (IM) -------------------------------------------
      # Apply IM on groups of event logs with dpi's from C and cur_dpi
      C_curdpi <- rbind(C, cur_dpi)
      trace_ids <- C_curdpi %>% pull(trace_id)
      discovered <- discover_pm(log, trace_ids, algo = algo)
      # GL <- log %>% edeaR::filter_trace(trace_ids = trace_ids)
      # PM <- pm4py::discovery_inductive(eventlog = GL, convert = T)


      # line 16: test conditions on fitness and cluster size --------------------
      # is log fitness >= target fitness?
      alignment <- pm4py::fitness_alignments(discovered$GL, marked_petrinet = discovered$PM)
      message("... with log_fitness = ", alignment$log_fitness)

      # size_dpi_C: curr. cluster size (absolute_frequency) WITHOUT candidate cur_dpi
      # size_dpi_R: absolute frequency of remaining traces INCLUDING candidate cur_dpi
      size_dpi_C <- tokenized_traces %>% filter(trace_id %in% C$trace_id) %>% pull(absolute_frequency) %>% sum() # returns 0 if C is empty
      size_dpi_R <- sum(R$absolute_frequency)

      if (alignment$log_fitness >= target_fitness) {
        message("... + C, - R")
        C <- C_curdpi
        R <- R %>% filter(trace_id != cur_dpi$trace_id)
      }


      # line 20: if cluster C is big enough -------------------------------------
      else if (size_dpi_C >= mcs * size_dpi_R) { # size_dpi_C > 0 ==> C is not empty
        cat("", sep = "\n")
        message("--- Phase 2: look ahead ---")
        message("evaluating remaining dpi\'s from R:")

        trace_ids <- C %>% pull(trace_id)
        discovered <- discover_pm(log, trace_ids, algo = algo)


        # line 22: Look ahead -----------------------------------------------------
        for (i in R$trace_id) { # i is a trace_id of dpi from R
          dpi <- R %>% filter(trace_id == i)
          GL_i <- log %>% edeaR::filter_trace(trace_ids = i) # fitness(i, PM)
          alignment <- pm4py::fitness_alignments(GL_i, marked_petrinet = discovered$PM)

          message("trace_id ", dpi$trace_id, " w/ fitness ", alignment$log_fitness)

          if (alignment$log_fitness >= target_fitness) {
            message("... + C, - R")

            C <- rbind(C, dpi)
            R <- R %>% filter(trace_id != dpi$trace_id)
          }
        }
        break
      }
      else {
        message("trace_id ",  cur_dpi$trace_id)
        message("... + I (ignored)")

        I <- I %>% rbind(cur_dpi)
      }
    }
    CS <- CS %>% append(list(C))
  }


  # line 37: Phase 3: residual trace resolution -----------------------------
  cat("", sep = "\n")
  message("--- Phase 3: residual trace resolution ---")

  if (nrow(R) > 0) { # if R is not empty

    # cluster all remaining traces into one cluster
    if (cluster_remaining_traces) {
      C <- R
      CS <- CS %>% append(list(R))

      message("remaining traces have been clustered together (cluster ", length(CS),")")
    }

    # distribute all remaining traces based on the best fitness with each cluster
    else {
      message("clustering remaining traces based on their fitness with the respective cluster:")

      trace_ids_list <- CS %>% map(~pull(., trace_id))
      tmpPMs <- purrr::map(trace_ids_list, ~discover_pm(log, trace_ids = .x, algo = algo)) %>% purrr::map(~.x[names(.x) == "PM"])
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
        message("trace_id ", trace_ids[i], " ... is added to the cluster ", better_fit_cluster)
      }
    }
  }

  # RETURN ORIGINAL LOG including both lifecycles (not only "complete")
  trace_ids <- CS %>% purrr::map(~pull(., trace_id))
  logs_list <- trace_ids %>% purrr::map(~filter_trace(log = log, trace_ids = .x))

  # assign attributes of list of logs
  # fitness
  PM_list <- logs_list %>% purrr::map(~discovery_inductive(.x, convert = T))
  fitness_list <- map2(logs_list, PM_list, ~pm4py::fitness_alignments(.x, marked_petrinet = .y, convert = T)) %>%
    purrr::map(as.data.frame)
  for (i in 1:length(fitness_list)) {fitness_list[[i]] <- fitness_list[[i]] %>% mutate(cluster = i)}

  fitness_list <- fitness_list %>% bind_rows()

  # precision
  precision_list <- map2(logs_list, PM_list, ~pm4py::fitness_alignments(.x, marked_petrinet = .y, convert = T)) %>%
    purrr::map(as.data.frame)
  for (i in 1:length(precision_list)) {precision_list[[i]] <- precision_list[[i]] %>% mutate(cluster = i)}
  precision_list <- precision_list %>% bind_rows()

  # total
  PM <- discovery_inductive(eventlog = log, convert = T)
  fitness_total <- fitness_alignments(eventlog = log, marked_petrinet = PM) %>%
    as.data.frame()
  total_precision <- precision_alignments(eventlog = log, marked_petrinet = PM)
  total <- cbind(fitness_total, total_precision)


  # assign attributes to ORIGINAL LOG
  original_logs <- trace_ids %>% purrr::map(~filter_trace(log = log_original, trace_ids = .x))

  attr(original_logs, "traces") <- CS
  attr(original_logs, "fitness") <- fitness_list
  attr(original_logs, "precision") <- precision_list
  attr(original_logs, "total") <- total
  # attr(logs_list, "fitness_total") <- fitness_total
  # attr(logs_list, "total_precision") <- total_precision

  # assign class
  class(original_logs) <- c("trace_clustering", class(original_logs))

  return(original_logs)
}
