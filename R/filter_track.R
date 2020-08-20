#' Filter and show count
#'
#' @param data dataset
#' @param ... other arguments to pass to filter
#'
#' @return
#' @export
#'
#' @examples
filter_track <- function(data, ...) {

  n_original <- nrow(data)

  data_filt <- filter(data, ...)

  n_new <- nrow(data_filt)
  n_removed <- n_original - n_new

  tibble(
    obs_type = c("kept", "removed", "total"),
    n = c(n_new, n_removed, n_original),
    prop = n / n_original
  ) %>%
    print()

  data_filt
}

