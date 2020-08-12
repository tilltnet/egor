#' [weights.egor()] extracts the (relative) sampling weights of each
#' ego in the dataset.
#'
#' @param object an [`egor`] object.
#' @template meth_dots
#'
#' @seealso \code{\link[survey]{weights.survey.design}}
#'
#' @export
#' @importFrom stats weights
weights.egor <- function(object, ...) {
  if(has_ego_design(object)) weights(object$ego)
  else rep(1, nrow(object$ego))
}

#' A helper function that takes an egor object and a list with arguments
#' to ego_design and runs survey::svydesign().
#'
#' @param egor an [`egor`] object (possibly missing design
#'   information).
#' @param ego_design a [`list`] of arguments to [a_survey_design()]
#'   specifying the sampling design for the egos. The arguments can
#'   refer to columns (ego attributes) of `egor`.
#' @param pos where the call to `as_survey_design`.
#'
#' @return If `ego_design` is a `list`, [`ego`] as a [`tbl_svy`]. If
#'   `NULL`, design information is not set or is cleared if present,
#'   returning a `tbl_df`.
#'
#' @noRd
.gen.ego_design <- function(egor, ego_design, pos=-1L){
  egos <- if(is(egor, "nested_egor")) egor else egor$ego
  if(is.null(ego_design)) return(as_tibble(egos))

  envir <- as.environment(pos)
#' @importFrom srvyr as_survey_design
  suppressWarnings(do.call(as_survey_design, c(list(egos), ego_design), envir=envir))
}

#' Set and query the ego sampling design
#'
#' Extract, set, remove, or update the survey design associated with
#' an ego-centered dataset.
#'
#' @param x an [`egor`] object.
#' @template meth_dots
#' @docType methods
#' @export
ego_design <- function(x, ...) UseMethod("ego_design")

#' @rdname ego_design
#' @export
ego_design.egor <- function(x, ...) if (has_ego_design(x)) x$ego # otherwise NULL

#' @rdname ego_design
#' @export
ego_design.nested_egor <- function(x, ...) if (has_ego_design(x)) x # otherwise NULL


#' @rdname ego_design
#' @export
`ego_design<-` <- function(x, ..., value) UseMethod("ego_design<-")

#' @rdname ego_design
#' @param value a [`list`] of arguments to [srvyr::as_survey_design()]
#'   specifying the sampling design for the egos. If the arguments are
#'   formulas, they can refer to columns (ego attributes) of
#'   `x`. `NULL` clears design information.
#'
#' @note This can be useful for adjusting or re-initializing the ego
#'   design information after the underlying ego attributes had been
#'   modified.
#' @export
`ego_design<-.egor` <- function(x, ..., value){
  x$ego <- .gen.ego_design(x, value, parent.frame())
  x
}

#' @rdname ego_design
#' @export
`ego_design<-.nested_egor` <- function(x, ..., value){
  x <- .gen.ego_design(x, value, parent.frame())
  class(x) <- c("nested_egor", class(x))
  x
}

#' @rdname ego_design
#' @export
has_ego_design <- function(x){
  UseMethod("has_ego_design")
}

#' @rdname ego_design
#' @export
has_ego_design.egor <- function(x){
  is(x$ego,"tbl_svy")
}

#' @rdname ego_design
#' @export
has_ego_design.nested_egor <- function(x){
  is(x,"tbl_svy")
}

#' @rdname ego_design
#' @export
strip_ego_design <- function(x){
  ego_design(x) <- NULL
  x
}
