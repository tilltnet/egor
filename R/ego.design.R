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
  weights(object$ego)
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
#' @noRd
.gen.ego_design <- function(egor, ego_design, pos=-1L){
  envir <- as.environment(pos)
#' @importFrom srvyr as_survey_design
  suppressWarnings(do.call(as_survey_design, c(list(egor$ego), ego_design), envir=envir))
}

#' Set and query the ego sampling design
#'
#' Extract, set, or update the survey design associated with an
#' ego-centered dataset.
#'
#' @param x an [`egor`] object.
#' @template meth_dots
#' @docType methods
#' @export
ego_design <- function(x, ...) UseMethod("ego_design")

#' @rdname ego_design
#' @export
ego_design.egor <- function(x, ...) attr(x, "ego_design")

#' @rdname ego_design
#' @export
`ego_design<-` <- function(x, ..., value) UseMethod("ego_design<-")

#' @rdname ego_design
#' @param value either `survey.design` object (like one constructed by
#'   [srvyr::as_survey_design()]) or a [`list`] of arguments to [srvyr::as_survey_design()]
#'   specifying the sampling design for the egos. If the arguments are
#'   formulas, they can refer to columns (ego attributes) of `x`.
#'
#' @note This can be useful for adjusting or reinitializing the ego
#'   design information after the underlying ego attributes had been
#'   modified.
#' @export
`ego_design<-.egor` <- function(x, ..., value){
  x$ego <- .gen.ego_design(x, value, parent.frame())
  x
}
