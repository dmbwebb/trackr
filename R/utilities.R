#' Prints all rows of a tibble
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
print_all <- function(data) {
  print(data, n = Inf)
}

#' Last non-NA in a vector
#' Calculate the last value in a vector that's not an NA, otherwise return NA
#'
#' @param x vector
#'
#' @return
#' @export
#'
#' @examples
last_non_na <- function(x) {
  x_no_na <- x[!is.na(x)]
  x_type <- typeof(x)
  x_class <- class(x)
  x_attr <- attributes(x)

  if (length(x_no_na) > 0) dplyr::last(x_no_na)

  else if (length(x_no_na) == 0) {

    if (x_type == "character") out <- NA_character_
    # else if (x_class == "Date") NA_Date_
    # else if (x_class == "factor") NA_factor_
    else if (x_type == "logical") out <- NA
    else if (x_type == "double") out <- NA_real_
    else if (x_type == "integer") out <- NA_integer_

    # Inherit class and attributes (useful for factors and dates)
    class(out) <- x_class
    attributes(out) <- x_attr

    return(out)

  }

}

#' First non-NA in a vector
#' Calculate the first value in a vector that's not an NA, otherwise return NA
#'
#' @param x vector
#'
#' @return
#' @export
#'
#' @examples
first_non_na <- function(x) {
  x_no_na <- x[!is.na(x)]
  x_type <- typeof(x)
  x_class <- class(x)
  x_attr <- attributes(x)

  if (length(x_no_na) > 0) dplyr::first(x_no_na)
  else if (length(x_no_na) == 0) {

    if (x_type == "character") out <- NA_character_
    # else if (x_class == "Date") NA_Date_
    # else if (x_class == "factor") NA_factor_
    else if (x_type == "logical") out <- NA
    else if (x_type == "double") out <- NA_real_
    else if (x_type == "integer") out <- NA_integer_

    # Inherit class and attributes (useful for factors and dates)
    class(out) <- x_class
    attributes(out) <- x_attr

    return(out)

  }
}

#' Calculate max of a vector or return NA if there are no non-missing values to calculate from
#' Useful for summarising
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
max_na <- function(...) {
  suppressWarnings(max_val <- max(..., na.rm = T))
  max_val[is.infinite(max_val)] <- NA_real_
  max_val
}

#' Calculate max of a vector or return NA if there are no non-missing values to calculate from
#' Useful for summarising
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
min_na <- function(...) {
  suppressWarnings(min_val <- min(..., na.rm = T))
  min_val[is.infinite(min_val)] <- NA_real_
  min_val
}

#' Calculate mean of a vector or return NA if there are no non-missing values to calculate from
#' Useful for summarising
#'
#' @param x vector
#'
#' @return
#' @export
#'
#' @examples
sum_na <- function(x) {
  l <- length(x[!is.na(x)])
  if (l > 0) sum(x, na.rm = T)
  else if (l == 0) NA
  else stop("something's wrong")
}

#' Calculate mean of a vector or return NA if there are no non-missing values to calculate from
#' Useful for summarising
#'
#' @param x vector
#'
#' @return
#' @export
#'
#' @examples
mean_na <- function(x) {
  l <- length(x[!is.na(x)])
  if (l > 0) mean(x, na.rm = T)
  else if (l == 0) NA
  else stop("something's wrong")
}

#' Calculate median of a vector or return NA if there are no non-missing values to calculate from
#' Useful for summarising
#'
#' @param x vector
#'
#' @return
#' @export
#'
#' @examples
median_na <- function(x) {
  l <- length(x[!is.na(x)])
  if (l > 0) median(x, na.rm = T)
  else if (l == 0) NA
  else stop("something's wrong")
}


#' Replace with NA if certain value
#'
#' @param x vector
#' @param values values to replace with NA
#'
#' @return
#' @export
#'
#' @examples
replace_with_na <- function(x, values) {
  ifelse(x %in% values, NA, x)
}
