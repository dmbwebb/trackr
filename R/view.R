#' Sample n groups in a grouped dataset
#'
#' @param data
#' @param n numeric, how many groups to sample
#'
#' @return
#' @export
#'
#' @examples
sample_n_groups <- function(data, n) {

  grouped <- dplyr::is_grouped_df(data)

  if (!grouped) {
    sampled_df <- dplyr::sample_n(data, n)
  } else if (grouped) {

    group_var <- dplyr::group_vars(data)
    warning(paste0("Showing ", n, " groups by ", paste0(group_var, collapse = ", ")))

    # Get N groups
    data_ids <- data %>%
      dplyr::ungroup() %>%
      dplyr::select(all_of(group_var)) %>%
      dplyr::distinct() %>%
      dplyr::sample_n(n)

    # Get dataaset with all info on those N groups
    sampled_df <- data %>%
      dplyr::semi_join(data_ids, by = group_var)

  }

  return(sampled_df)

}








#' Randomly view N rows
#'
#' @param data data.frame or tibble
#' @param n How many rows (or groups) to view
#' @param by_group If TRUE, then view n randomly selected groups, if false then view n randomly selected rows
#'
#' @return
#' @export
#'
#' @examples
view_n <- function(data, n = 200, by_group = TRUE) {

  # For testing
  # x <- d; by_group = T; n = 200
  # x <- exp_harvest_weather %>% group_by(hhid_unique)

  # if (round) x <- x %>%
  #     mutate(across(where(is.numeric),
  #                   ~ signif(., 3)))

  grouped <- dplyr::is_grouped_df(data)

  if (!grouped | !by_group) {

    show_all <- nrow(data) <= n

    if (show_all) {
      n_updated <- nrow(data)
    } else if (!show_all) {
      n_updated <- n
    }

    if (!by_group & grouped) {
      warning("Not showing by groups")
    }

    tibble::view(sample_n(data, n_updated))

  } else if (grouped & by_group) {

    # # mean_group_size <- mean(group_size(x))
    # group_var <- group_vars(x)
    #
    # warning(paste0("Showing ", n, " groups by ", paste0(group_var, collapse = ", ")))
    #
    # # n_group_guess <- ceiling(n / mean_group_size)
    #
    # # Get N groups
    # x_ids <- x %>%
    #   ungroup() %>%
    #   select(all_of(group_var)) %>%
    #   distinct() %>%
    #   sample_n(n)
    #
    # # Get dataset with all info on those N groups
    # sampled_df <- x %>%
    #   semi_join(x_ids, by = group_var)

    sampled_df <- sample_n_groups(data, n)

    tibble::view(sampled_df)

  }

  invisible(data)

}


#' View a randomly selected sample of observations after applying a filter
#'
#' @param data data.frame or tibble
#' @param ... arguments to pass to filter
#' @param n number of
#' @param by_group If TRUE, then view n randomly selected groups, if false then view n randomly selected rows
#'
#' @return
#' @export
#'
#' @examples
view_filter <- function(data, ..., n = 200, by_group = TRUE) {

  data %>%
    dplyr::filter(...) %>%
    view_n(n = n, by_group = by_group)

  invisible(data)

}

#' View a randomly selected sample of observations after selecting only some variables to view
#'
#' @param data data.frame or tibble
#' @param ... arguments to pass to select
#' @param n number of observations to view
#' @param by_group If TRUE, then view n randomly selected groups, if false then view n randomly selected rows
#'
#' @return
#' @export
#'
#' @examples
view_select <- function(data, ..., n = 200, by_group = TRUE) {

  data %>%
    dplyr::select(...) %>%
    view_n(n = n, by_group = by_group)

  invisible(data)

}
