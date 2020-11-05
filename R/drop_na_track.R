#' drop_na and see how many you drop
#'
#' @param data
#' @param ... arguments to pass to drop_na
#'
#' @return
#' @export
#'
#' @examples
drop_na_track <- function(data, ...) {

  dat_dropped <- drop_na(data, ...)

  n_old <- nrow(data)
  n_new <- nrow(dat_dropped)

  tibble(
    obs_type = c("kept", "removed", "total"),
    n = c(n_new, n_old - n_new, n_old),
    prop = n / n_old
  ) %>%
    print()

  dat_dropped

}
