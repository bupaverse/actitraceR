#' Create a dictionary of activity id's
#'
#' Creates a dictionary of unique activity id's.
#'
#' @param log [`log`] An object of type [`log`].
#' @return [list] A dictionary object of type `list`.
#'
#' @export
get_dict <- function(log) {
  dict <- log %>% pull(bupaR::activity_id(log)) %>% as.character() %>% unique() %>% as.list
  dict
}
