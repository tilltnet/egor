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
  attributes(result)$ego_design$variables <- new_variables
  result
}

#' Trims alter-alter ties of alters that are missing/ deleted from alters data
#' 
#' @param object An `egor` object.
#' @return An `egor` object with trimmed alter-alter ties (.aaties).
#' @export
trim_aaties <- function(object) {
  object$aatie <-
    purrr::map_dfr(split(object$aatie, object$aatie$.egoID),
                   function(x) {
                     if (nrow(x) > 0) {
                       alter_subset <- filter(object$alter, .egoID == x$.egoID[[1]])
                       
                       filter(
                         x,
                         .egoID %in% object$ego$.egoID,
                         .srcID %in% alter_subset$.altID,
                         .tgtID %in% alter_subset$.altID
                       )
                     } else x
                   })
  
  object
}

#' @export
#' @noRd
#' @method mutate egor
mutate.egor <- function(.data, ...) {
  result <- mutate(.data[[attr(.data, "active")]], ...)
  .data[[attr(.data, "active")]] <- result
  .data <- trim_aaties(.data)
  trim_alters(.data)
}

#' Trims alters that are missing/ deleted from ego data
#' 
#' @param object An `egor` object.
#' @return An `egor` object with trimmed alter-alter ties (.aaties).
#' @export
trim_alters <- function(object) {
  object$alter <-
    filter(object$alter, .egoID %in% object$ego$.egoID)
  object
}
  
#' @export
#' @noRd
#' @method transmute egor
transmute.egor <- function(.data, ...) {
  result <- transmute(.data[[attr(.data, "active")]], ...)
  result <- 
    bind_cols(
      select(.data[[attr(.data, "active")]],
             ends_with("ID")),
      result
    )

  .data[[attr(.data, "active")]] <- result
  .data <- trim_aaties(.data)
  trim_alters(.data)
}

#' @export
#' @noRd
#' @method select egor
select.egor <- function(.data, ...) {
  result <- select(.data[[attr(.data, "active")]], ...)
  result <- 
    bind_cols(
      select(.data[[attr(.data, "active")]],
             ends_with("ID")),
      result
    )
  .data[[attr(.data, "active")]] <- result
  .data <- trim_aaties(.data)
  trim_alters(.data)
}

#' @export
#' @noRd
#' @method select egor
rename.egor <- function(.data, ...) {
  result <- rename(.data[[attr(.data, "active")]], ...)
  result <- 
    bind_cols(
      select(.data[[attr(.data, "active")]],
             ends_with("ID")),
      result
    )
  .data[[attr(.data, "active")]] <- result
  .data <- trim_aaties(.data)
  trim_alters(.data)
}

#' @export
#' @noRd
#' @method filter egor
filter.egor <- function(.data, ...) {
  result <- filter(.data[[attr(.data, "active")]], ...)
  .data[[attr(.data, "active")]] <- result
  .data <- trim_aaties(.data)
  trim_alters(.data)
}

#' @export
#' @noRd
#' @method group_by egor
group_by.egor <- function(.data, ...) {
  result <- group_by(.data[[attr(.data, "active")]], ...)
  .data[[attr(.data, "active")]] <- result
  .data
}

#' @export
#' @noRd
#' @method count egor
count.egor <- function(.data, ...) {
  count(.data[[attr(.data, "active")]], ...)
}

#' @export
#' @noRd
#' @method summarise egor
summarise.egor <- function(.data, ...) {
  summarise(.data[[attr(.data, "active")]], ...)
}

#' @export
#' @noRd
#' @method arrange egor
arrange.egor <- function(.data, ...) {
  result <- arrange(.data[[attr(.data, "active")]], ...)
  .data[[attr(.data, "active")]] <- result
  .data <- trim_aaties(.data)
  trim_alters(.data)
}

#' @export
#' @noRd
#' @method inner_join egor
inner_join.egor <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  result <- inner_join(x[[attr(x, "active")]], y, by = by, copy = copy, suffix = suffix, ...)
  x[[attr(.data, "active")]] <- result
  x <- trim_aaties(x)
  trim_alters(x)
}

#' @export
#' @noRd
#' @method left_join egor
left_join.egor <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  result <- left_join(x[[attr(x, "active")]], y, by = by, copy = copy, suffix = suffix,...)
  x[[attr(.data, "active")]] <- result
  x <- trim_aaties(x)
  trim_alters(x)
}

#' @export
#' @noRd
#' @method right_join egor
right_join.egor <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  result <- right_join(x[[attr(x, "active")]], y, by = by, copy = copy, suffix = suffix,...)
  x[[attr(.data, "active")]] <- result
  x <- trim_aaties(x)
  trim_alters(x)
}

#' @export
#' @noRd
#' @method full_join egor
full_join.egor <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  result <- full_join(x[[attr(x, "active")]], y, by = by, copy = copy, suffix = suffix,...)
  x[[attr(.data, "active")]] <- result
  x <- trim_aaties(x)
  trim_alters(x)
}

#' @export
#' @noRd
#' @method semi_join egor
semi_join.egor <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  result <- semi_join(x[[attr(x, "active")]], y, by = by, copy = copy, suffix = suffix,...)
  .data[[attr(.data, "active")]] <- result
  .data <- trim_aaties(.data)
  trim_alters(.data)
}

#' @export
#' @noRd
#' @method nest_join egor
nest_join.egor <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  result <- nest_join(x[[attr(x, "active")]], y, by = by, copy = copy, suffix = suffix,...)
  .data[[attr(.data, "active")]] <- result
  .data <- trim_aaties(.data)
  trim_alters(.data)
}

#' @export
#' @noRd
#' @method anti_join egor
anti_join.egor <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  result <- anti_join(x[[attr(x, "active")]], y, by = by, copy = copy, suffix = suffix,...)
  .data[[attr(.data, "active")]] <- result
  .data <- trim_aaties(.data)
  trim_alters(.data)
}

# missing:
# binds (bind_rows, bind_cols) - vielleicht nicht sinnvoll machbar

# add function that deletes edges that invole alters that have been deleted.