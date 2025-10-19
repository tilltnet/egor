if (getRversion() >= "2.15.1")
  utils::globalVariables(c(".tmp_row_id", "ego_id", "V1", "V2"))

# egor conversions



#' Extract ego, alter, and alter-alter tables from an `egor` object.
#'
#' @description Provided an `egor` object, these functions create a "global" `tibble` or `srvyr`'s [`tbl_svy`] object
#' containing egos, alter attributes, or alter-alter relations. The resulting tables
#' are useful for advanced analysis procedures, e.g. multi-level regressions.
#'
#' @description [as_tibble()] method for `egor` extracts the currently active component (`ego`, `alter`, or `aaties`) table, optionally joining it with the others, dropping any survey design information.
#'
#' @param x,object,.data An `egor` object.
#' @param include.ego.vars Logical, specifying if ego variables should be included in the result.
#' @param include.alter.vars Logical, specifying if alter variables should be included in the result.
#' @param ... Additional arguments, currently unused.
#' @examples
#' # Load example data
#' data(egor32)
#'
#' as_tibble(egor32) # Ego table.
#'
#' egor32 %>%
#'  activate("alter") %>%
#'  as_tibble(include.ego.vars=TRUE) # Alter table, but also with ego variables.
#'
#' @return A `tibble` for the `as_tibble` and `*_df` functions and a `tbl_svy` for `as_survey` and the `*_survey` functions.
#' @export
as_tibble.egor <- function(x,
                           ...,
                           include.ego.vars = FALSE,
                           include.alter.vars = FALSE) {
  res <- as_tibble(x[[attr(x, "active")]])
  
  if (include.ego.vars && attr(x, "active") != "ego") {
    ego <- if (has_ego_design(x))
      x$ego$variables
    else
      x$ego
    
    names(ego)[names(ego) != ".egoID"] <-
      paste0(names(ego)[names(ego) != ".egoID"] , "_ego")
    res <- full_join(res, ego, by = ".egoID")
  }
  
  if (include.alter.vars & attr(x, "active") == "aatie") {
    res <- left_join(res,
                     x$alter,
                     by = c(".egoID", ".srcID" = ".altID"))
    res <- left_join(
      res,
      x$alter,
      by = c(".egoID", ".tgtID" = ".altID"),
      suffix = c("_src", "_tgt")
    )
  }
  
  res
}

#' @rdname as_tibble.egor
#' @description [as_survey()] method for `egor` instead returns a `srvyr` [`tbl_svy`] survey, taking into account any replication due to multiple alters or alter-alter ties incident on each ego. If no design is specified for the egos, then the default design (simple random sample with replacement) is assumed as the starting point.
#' @examples
#' library(srvyr)
#' as_survey(egor32) # Ego table with survey design.
#' @importFrom srvyr as_survey
#' @export
as_survey.egor <- function(.data,
                           ...,
                           include.ego.vars = FALSE,
                           include.alter.vars = FALSE) {
  if (!has_ego_design(.data))
    .data$ego <- as_survey(.data$ego)
  # Obtain the results ignoring design.
  result <- as_tibble(.data,
                      ...,
                      include.ego.vars = include.ego.vars,
                      include.alter.vars = include.alter.vars)
  # Now, figure out to which original ego row each of the output rows corresponds.
  emap <- match(result$.egoID, .data$ego$variables$.egoID)
  # Augment the initial ego survey design
  result.design <- .data$ego[emap,]
  result.design$variables <- result
  result.design
}

#' @rdname as_tibble.egor
#' @description `as_egos_df()`, `as_alters_df()`, `as_aaties_df()`, `as_egos_survey()`, `as_alters_survey()`, and `as_aaties_survey()` are convenience functions for the `as_tibble()` and `as_survey()` methods, activating the corresponding component of the `egor` object.
#' @examples
#'
#' # Despite alter table being active, obtain the ego table.
#' (egor32 <- activate(egor32, "alter"))
#' as_egos_df(egor32)
#'
#' @export
as_egos_df <- function(object) {
  object <- activate(object, "ego")
  as_tibble(object)
}

#' @rdname as_tibble.egor
#' @examples
#' # Create global alter table
#' as_alters_df(egor32)
#'
#' @export
as_alters_df <- function(object, include.ego.vars = FALSE) {
  object <- activate(object, "alter")
  as_tibble(object, include.ego.vars = include.ego.vars)
}

#' @rdname as_tibble.egor
#' @examples
#' # Create global alter-alter relations table
#' as_aaties_df(egor32)
#'
#' # ... adding alter variables
#' as_aaties_df(egor32, include.alter.vars = TRUE)
#' @export
#' @importFrom dplyr left_join
#' @importFrom purrr map_lgl
as_aaties_df <- function(object,
                         include.ego.vars = FALSE,
                         include.alter.vars = FALSE) {
  object <- activate(object, "aatie")
  as_tibble(object,
            include.ego.vars = include.ego.vars,
            include.alter.vars = include.alter.vars)
}

#' @rdname as_tibble.egor
#' @examples
#' as_egos_survey(egor32)
#' @export
as_egos_survey <- function(object, include.ego.vars = FALSE) {
  object <- activate(object, "ego")
  as_survey(object, include.ego.vars = include.ego.vars)
}

#' @rdname as_tibble.egor
#' @examples
#' as_alters_survey(egor32) # Notice the resulting cluster design.
#' @export
as_alters_survey <- function(object, include.ego.vars = FALSE) {
  object <- activate(object, "alter")
  as_survey(object, include.ego.vars = include.ego.vars)
}

#' @rdname as_tibble.egor
#' @export
as_aaties_survey <- function(object,
                             include.ego.vars = FALSE,
                             include.alter.vars = FALSE) {
  object <- activate(object, "aatie")
  as_survey(object,
            include.ego.vars = include.ego.vars,
            include.alter.vars = include.alter.vars)
}
