#' Join two datasets and see how many matches there are
#'
#' @param x dataset
#' @param y dataset
#' @param by A character vector of variables to join by
#' @param suffix If there are non-joined duplicate variables in x and y, these suffixes will be added to the output to disambiguate them. Should be a character vector of length 2.
#' @param .merge If TRUE, then joined dataset will contain a variable called .merge that indicates which dataset the row came from
#' @param join_type Which type of join? (full_join, left_join, right_join, inner_join)
#' @param ... Other functions passed to the _join function
#'
#' @return
#' @export
#'
#' @examples
join_track <- function(x, y, by = NULL, suffix = c(".x", ".y"),
                      .merge = FALSE, join_type, ...) {

  # Make sure "by" reverses properly
  if (!is.null(names(by))) {
    by_cols_df <- by %>%
      tibble::enframe() %>%
      rlang::set_names(c("l", "r")) %>%
      dplyr::mutate(dplyr::across(tidyselect::everything(), haven::zap_empty)) %>%
      dplyr::mutate(l = dplyr::coalesce(l,r),
             r = dplyr::coalesce(r, l))

    by_cols_l <- by_cols_df %>% tibble::deframe()
    by_cols_r <- by_cols_df %>% dplyr::select(r, l) %>% tibble::deframe()
  } else if (is.null(names(by))) {
    by_cols_l <- by
    by_cols_r <- by
  }

  # Checking to make sure used variable names are not already in use
  if(".x_tracker" %in% names(x)){
    message("Warning: variable .x_tracker in left data was dropped")
  }
  if(".y_tracker" %in% names(y)){
    message("Warning: variable .y_tracker in right data was dropped")
  }
  if(.merge & (".merge" %in% names(x) | ".merge" %in% names(y))){
    stop("Variable .merge already exists; change name before proceeding")
  }

  # Adding simple merge tracker variables to data frames
  x[, ".x_tracker"] <- 1
  y[, ".y_tracker"] <- 1

  # Doing full join
  joined <- join_type(x, y, by = by_cols_l, suffix = suffix,  ...)

  # Calculating merge diagnoses
  matched <- joined %>%
    dplyr::filter(!is.na(.x_tracker) & !is.na(.y_tracker)) %>%
    nrow()
  x_only <- joined %>%
    dplyr::filter(!is.na(.x_tracker) & is.na(.y_tracker)) %>%
    nrow()
  y_only <- joined %>%
    dplyr::filter(is.na(.x_tracker) & !is.na(.y_tracker)) %>%
    nrow()

  counts <- tibble::tibble(merge_status = c("x_only", "y_only", "matched"),
                           n = c(x_only, y_only, matched),
                           prop = n / sum(n))

  print(counts)

  # Create .merge variable if specified
  if(.merge){
    joined <- joined %>%
      mutate(.merge =
               dplyr::case_when(
                 !is.na(.$.x_tracker) & is.na(.$.y_tracker) ~ "x_only",
                 is.na(.$.x_tracker) & !is.na(.$.y_tracker) ~ "y_only",
                 TRUE ~ "matched"
               )
      )
  }

  # Dropping tracker variables and returning data frame
  joined <- joined %>%
    dplyr::select(-.x_tracker, -.y_tracker)

  return(joined)

}






#' full_join that shows how many rows match
#'
#' @param x
#' @param y
#' @param by
#' @param suffix
#' @param .merge
#' @param ...
#'
#' @return
#' @export
#' @inheritParams join_track
#'
#' @examples
full_join_track <- function(x, y, by = NULL, suffix = c(".x", ".y"),
                            .merge = FALSE, ...) {

  join_track(x = x, y = y, by = by, suffix = suffix,
             .merge = .merge, join_type = dplyr::full_join, ...)

}

#' left_join that shows how many rows match
#'
#' @param x
#' @param y
#' @param by
#' @param suffix
#' @param .merge
#' @param ...
#'
#' @return
#' @export
#'
#' @inheritParams join_track
#' @examples
left_join_track <- function(x, y, by = NULL, suffix = c(".x", ".y"),
                            .merge = FALSE, ...) {

  join_track(x = x, y = y, by = by, suffix = suffix,
             .merge = .merge, join_type = dplyr::left_join, ...)

}

#' right_join that shows how many rows match
#'
#' @param x
#' @param y
#' @param by
#' @param suffix
#' @param .merge
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
right_join_track <- function(x, y, by = NULL, suffix = c(".x", ".y"),
                            .merge = FALSE, ...) {

  join_track(x = x, y = y, by = by, suffix = suffix,
             .merge = .merge, join_type = dplyr::right_join, ...)

}

#' inner_join that shows how many rows match
#'
#' @param x
#' @param y
#' @param by
#' @param suffix
#' @param .merge
#' @param ...
#'
#' @return
#' @export
#'
#' @inheritParams join_track
#'
#' @examples
inner_join_track <- function(x, y, by = NULL, suffix = c(".x", ".y"),
                             .merge = FALSE, ...) {

  join_track(x = x, y = y, by = by, suffix = suffix,
             .merge = .merge, join_type = dplyr::inner_join, ...)

}








#
#
# d1 <- tibble(x1 = sample(1:100, 100, replace = TRUE),
#              y = sample(1:100, 100, replace = TRUE))
#
# d2 <- tibble(x2 = sample(1:100, 200, replace = TRUE),
#              y = sample(1:100, 200, replace = TRUE),
#              z = sample(1:100, 200, replace = TRUE))
#
# right_join_track(d1, d2, by = c("x1" = "x2"), .merge = TRUE)
