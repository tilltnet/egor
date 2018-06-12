# dplyr methods
restore_egor_attributes <- function(result, egor_obj) {
  attrs_old <- attributes(egor_obj)
  attrs_old <- attrs_old[!names(attrs_old) %in% c("names", "row.names")]
  attrs_new <- attributes(result)
  attrs_new <- attrs_new[!names(attrs_new) %in% names(attrs_old)]
  attributes(result) <-  c(attrs_old, attrs_new)
  update_ego_design(result)
}

update_ego_design <- function(result) {
  new_variables <- as_tibble(result)
  attr_keep <- attributes(new_variables)
  attr_keep <- attr_keep[names(attr_keep) %in% c("names", "row.names", "class")]
  attributes(new_variables) <- attr_keep
  attributes(result)$ego.design$variables <- new_variables
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

# select seems to maintain attributes already
#' @export
#' @noRd
select.egor <- function(.data, ...) {
  result <- NextMethod()
  restore_egor_attributes(result, .data)
}

#' @export
#' @noRd
filter.egor <- function(.data, ...) {
  lrow_ix <- NROW(.data)
  #' @importFrom dplyr bind_cols
  .data <- bind_cols(.data, tmp_ix__ = 1:lrow_ix)
  result <- NextMethod()
  result <- restore_egor_attributes(result, .data)
  ed <- attributes(result)$ego.design
  attributes(result)$ego.design <- ed[result$tmp_ix__, ]
  #' @importFrom dplyr select
  select(result, -tmp_ix__)
}

#' @export
#' @noRd
group_by.egor <- function(.data, ...) {
  result <- NextMethod()
  result <- restore_egor_attributes(result, .data)
  class(result) <- c("grouped_df", class(result))
  result
}

# missing:
# joins (full_join, etc)
# binds (bind_rows, bind_cols)