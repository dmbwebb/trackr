#' Count the number of NAs in each variable
#'
#' @param data dataset
#' @param sort logical. If TRUE, the final count will be sorted by the proportion of NAs
#' @param return_count logical. If FALSE, the count is printed, and the dataset is invisibly returned. If TRUE, the count itself is returned.
#'
#' @return
#' @export
#'
#' @examples
count_nas <- function(data, sort = FALSE, return_count = FALSE) {
  nas <- colSums(is.na(data)) %>% enframe
  vals <- colSums(!is.na(data)) %>% enframe

  both <- nas %>%
    dplyr::inner_join(vals, by = "name") %>%
    dplyr::select(variable = name,
                  missing = value.x,
                  non_missing = value.y) %>%
    dplyr::mutate(prop_missing = missing / (missing + non_missing))

  if (sort) {
    both <- both %>% dplyr::arrange(-prop_missing)
  }

  both %>% print(n = Inf)

  if (!return_count) invisible(data)
  else if (return_count) both

}

#' Count in middle of a pipeline and print
#'
#' @param data dataset
#' @param ... other arguments to pass from count
#'
#' @return
#' @export
#'
#' @examples
count_print <- function(data, ...) {
  dplyr::count(data, ...) %>% print_all

  invisible(data)
}


#' Create a relative frequency table and print
#'
#' @param data dataset
#' @param ... other arguments to pass to
#' @param return_count logical. If FALSE, the count is printed, and the dataset is invisibly returned. If TRUE, the count itself is returned.
#'
#' @return
#' @export
#'
#' @examples
count_prop <- function(data, ..., return_count = FALSE) {
  count_table <- data %>% dplyr::count(...) %>%
    dplyr::mutate(prop = n / sum(n)) %>%
    print_all()

  if (return_count) return(count_table)
  else if (!return_count) invisible(data)

}
