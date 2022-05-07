#
# df <- tibble(
#   x = sample(1:100, 40, replace = TRUE),
#   y = sample(1:100, 40, replace = TRUE)
# )
#
# edited_vars <- df %>% dplyr:::mutate_cols(x = if_else(y > 40, 0L, x)) %>% names()
#
# new_df <- df %>%
#   mutate(x = if_else(y > 40, 0L, x)) %>%
#   mutate(x = if_else(y < 30, NA_integer_, x))
#
# harsh_equal <- function(x, y) {
#   x == y | (is.na(x) & !is.na(y)) | (!is.na(x) & is.na(y))
# }
#
# for (var in edited_vars) {
#   old_x <- df[[var]]      # this will return null if it doesn't exist in old df
#   new_x <- new_df[[var]]
#
#   n_changes <- sum(harsh_equal(old_x, new_x))
#   n_new_nas <- sum(!is.na(df$x) & is.na(new_df$x))
#
#   if (is.null(old_x)) {
#     print(paste0(var, " is a new variable"))
#   } else {
#     print(paste0(var, ": ", n_changes, " changes were made"))
#     print(paste0(var, ": ", n_new_nas, " rows were changed to NA"))
#   }
# }
#
# df[["z"]]
# n_changes <- sum(harsh_equal(df$x, new_df$x))
# n_new_nas <- sum(!is.na(df$x) & is.na(new_df$x))
#
#
# ?mutate
# mutate
# mutate.data.frame
# getS3method("mutate", "data.frame")
# dplyr:::mutate_cols



#' Track what changes are made when you mutate
#'
#' @param .data dataset
#' @param ... arguments to pass to mutate
#'
#' @return
#' @export
#'
#' @examples
mutate_track <- function(.data, ...) {

  new_df <- mutate(.data, ...)

  edited_vars <- dplyr:::mutate_cols(.data, dplyr:::dplyr_quosures(...), caller_env = rlang::caller_env()) %>% names()

  harsh_equal <- function(x, y) {
    st_equal <- x == y
    both_nas <- is.na(x) & is.na(y)
    dplyr::if_else(is.na(st_equal), both_nas, st_equal)
  }

  for (var in edited_vars) {
    old_x <- .data[[var]]     # this will return null if it doesn't exist in old df
    new_x <- new_df[[var]]

    n_changes <- sum(!harsh_equal(old_x, new_x))
    n_new_nas <- sum(!is.na(old_x) & is.na(new_x))

    if (is.null(old_x)) {
      print(paste0(var, " is a new variable"))
    } else {
      print_string <- paste0(var, ": ", n_changes, " changes made")
      if (n_new_nas > 0) print_string <- paste0(print_string, ", ", n_new_nas, " rows changed to NA")

      print(print_string)
    }
  }

  return(new_df)

}

tibble(
  a = sample(1:100, 100, replace = TRUE),
  b = sample(1:100, 100, replace = TRUE)
) %>%
  mutate_track(a = ifelse(b > 50, 20, a)) %>%
  mutate_track(a = ifelse(b < 20, NA, a)) %>%
  mutate_track(b = a)


