# dplyr methods
restore_egor_attributes <- function(result, egor_obj) {
  attrs_old <- attributes(egor_obj)
  attrs_old <- attrs_old[!names(attrs_old) %in% c("names", "row.names")]
  attrs_new <- attributes(result)
  attrs_new <- attrs_new[!names(attrs_new) %in% names(attrs_old)]
  attributes(result) <-  c(attrs_old, attrs_new)
  result
}

#' @export
#' @noRd
mutate.egor <- function(.data, ...) {
  result <- NextMethod()
  restore_egor_attributes(result, .data)
}

#' @export
#' @noRd
transmute.egor <- function(.data, ...) {
  result <- NextMethod()
  restore_egor_attributes(result, .data)
}

# select seems to keep attributes already
#' @export
#' @noRd
select.egor <- function(.data, ...) {
  result <- NextMethod()
  restore_egor_attributes(result, .data)
}

#' @export
#' @noRd
filter.egor <- function(.data, ...) {
  result <- NextMethod()
  restore_egor_attributes(result, .data)
}

#' @export
#' @noRd
summarise.egor <- function(.data, ...) {
  result <- NextMethod()
  restore_egor_attributes(result, .data)
}

#' @export
#' @noRd
summarize.egor <- summarise.egor

#' @export
#' @noRd
group_by.egor <- function(.data, ...) {
  result <- NextMethod()
  result <- restore_egor_attributes(result, .data)
  class(result) <- c("grouped_df", class(result))
  result
}