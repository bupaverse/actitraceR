
#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom stringr str_count str_length str_c str_extract_all fixed str_locate_all str_sub str_detect
#' @importFrom purrr cross_df map_lgl map map_chr
#' @importFrom tidyr unite nest spread
#'
#'
# Getting traces from R\I based on a window size --------------------------
get_window_traces <- function(R, I, window_size) {

  trace_id <- absolute_frequency <- relative_frequency <- cum_rel_freq <- NULL

  # R: [tbl_df] set of remaining dpi's
  # I: [chr] set of ignored dpi's (or trace_id's)
  W <- R %>% dplyr::filter(!(trace_id %in% I$trace_id))
  W <- W %>% arrange(desc(absolute_frequency))
  W <- W %>% mutate(cum_rel_freq = cumsum(relative_frequency)) %>% filter(lag(cum_rel_freq, default = 0) <= window_size)
  W <- W %>% select(-cum_rel_freq)
  return(W)
}


# Discover PM w/ GL -------------------------------------------------------
discover_pm <- function(log, trace_ids, algo, ...) { # cluster from C
  GL <- log %>% edeaR::filter_trace(trace_ids = trace_ids)
  PM <- algo(eventlog = GL, convert = T, ...)
  return(list(GL = GL, PM = PM))
}


# Calculate fitness -------------------------------------------------------
calc_fitness <- function(PM, log, trace_id) { # unique trace_id from R
  GL <- log %>% edeaR::filter_trace(trace_ids = trace_id)
  pm4py::fitness_alignments(GL, PM)
}


# Tokenize trace_list -----------------------------------------------------
to_token <- function(trace_list, dict) {
  trace_tokenized <- purrr::map(as.list(trace_list), ~which(.x == dict)) %>% unlist() %>% paste0("[", ., "]")
  trace_tokenized
}


# Calculate distance (mra/stringdist) -------------------------------------
shortest_dist_dpi <- function(dict, C, W, distance_metric, equivalence_classes) {
  tok_C <- C$tokens %>% purrr::map(paste, collapse = "")

  avg_distances <- numeric(length = nrow(W))

  for (i in 1:nrow(W)) {
    tok_W <- W$tokens[[i]] %>% paste(collapse = "")

    if(str_detect(deparse(distance_metric[[1]]), "stringdist")) {
      distances <- call(deparse(distance_metric[[1]]), a = tok_C %>% unlist(), b = tok_W, unlist(as.list(distance_metric[-1]))) %>% eval()
    } else if(str_detect(deparse(distance_metric[[2]]), "mra")) {
      distances <- call(deparse(distance_metric[[1]]), a = tok_C, b = tok_W, equivalence_classes, unlist(as.list(distance_metric[-1]))) %>% eval()
    }

    distances %>% unlist %>% mean -> avg_dist
    avg_distances[i] <- avg_dist
  }

  cur_dpi <- W[avg_distances %>% which.min(), ]
  return(cur_dpi)
}


# MRA distance method -----------------------------------------------------
is_maximal_repeat <- function(trace, candidate) {

  start <- end <- before <- after <- NULL

  if(str_count(trace, fixed(candidate)) > 1) {
    str_locate_all(trace, fixed(candidate))[[1]] %>%
      as_tibble() %>%
      mutate(before = str_sub(trace, start-1, start-1),
             after = str_sub(trace, end+1, end+1)) %>%
      count(before, after)%>%
      spread(after, n)  -> tmp

    nrow(tmp) >= 2 & ncol(tmp) >= 3
  } else {
    FALSE
  }
}


# Create candidates -------------------------------------------------------
create_candidates <- function(max_length, full_log_trace, activities) {

  candidate <- NULL

  tibble(candidate = activities, length = 1) %>%
    mutate(count = str_count(full_log_trace, fixed(candidate))) %>%
    filter(count >= 2) -> candidates

  if(max_length > 1)
    for(l in 2:max_length) {
      length_min_1 <- candidates %>%
        filter(length == l - 1) %>%
        pull(candidate)
      activities <- candidates %>%
        filter(length == 1) %>%
        pull(candidate)

      new_candidates <-
        bind_rows(
          cross_df(list(v1 = length_min_1, v2 = activities)) ,
          cross_df(list(v1 = activities, v2 = length_min_1))
        ) %>%
        unite(candidate, sep = "")  %>%
        distinct() %>%
        mutate(count = str_count(full_log_trace, fixed(candidate)))  %>%
        filter(count >= 2) %>%
        mutate(length = l)

      if(nrow(new_candidates) == 0)
        break

      candidates <- bind_rows(candidates, new_candidates)
    }
  return(candidates)
}


# Get alphabet set --------------------------------------------------------
get_alphabet_set <- function(max_repeat, activities) {

  sort(unique(str_extract_all(max_repeat, fixed(activities)) %>% unlist()))
}


# Get MRA -----------------------------------------------------------------
get_MRA <- function(candidates, full_log_trace, activities) {

  candidate <- alphabet <- alphabet_char <- NULL

  max_repeats <- candidates %>%
    mutate(is_maximal_repeat = map_lgl(candidate, ~is_maximal_repeat(full_log_trace, .x))) %>%
    filter(is_maximal_repeat)

  max_repeats %>%
    filter(str_length(candidate) > 1) -> MR

  MR %>%
    mutate(alphabet = map(candidate, get_alphabet_set, activities)) %>%
    mutate(alphabet_char = map_chr(alphabet, str_c, collapse = "")) -> MRA

  MRA %>%
    select(alphabet_char, candidate) %>%
    group_by(alphabet_char) %>%
    nest() -> equivalence_classes_df

  equivalence_classes <- map(equivalence_classes_df$data, ~.x$candidate)
  names(equivalence_classes) <- equivalence_classes_df$alphabet_char
  return(equivalence_classes)
}






