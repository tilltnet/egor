if(getRversion() >= "2.15.1") utils::globalVariables(c("tmp_ix__", ".srcID", ".tgtID"))

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
#' @method mutate egor
mutate.egor <- function(.data, ...) {
  result <- NextMethod()
  # Filter .aaties where srcID and tgtID are missing/deleted from .alts
  result <- trim_aaties(result)
  
  restore_egor_attributes(result, .data)
}

#' Trims alter-alter ties of alters that are missing/ deleted from alters data
#' 
#' @param object An `egor` object.
#' @return An `egor` object with trimmed alter-alter ties (.aaties).
#' @export
trim_aaties <- function(object) {
  object$.aaties <- map2(object$.alts, object$.aaties, function(alts, aaties) {
    filter(aaties, .srcID %in% alts$.altID | .tgtID %in% alts$.altID)
  })
  object
}
  
#' @export
#' @noRd
#' @method transmute egor
transmute.egor <- function(.data, ...) {
  result <- NextMethod()
  restore_egor_attributes(result, .data)
}

# select seems to maintain attributes already
#' @export
#' @noRd
#' @method select egor
select.egor <- function(.data, ...) {
  result <- NextMethod()
  restore_egor_attributes(result, .data)
}

#' @export
#' @noRd
#' @method filter egor
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
#' @method group_by egor
group_by.egor <- function(.data, ...) {
  result <- NextMethod()
  result <- restore_egor_attributes(result, .data)
  class(result) <- c("grouped_df", class(result))
  result
}

# missing:
# joins (full_join, etc)
# binds (bind_rows, bind_cols)

# add function that deletes edges that invole alters that have been deleted.