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
  weights(attr(object,"ego_design"), ...)
}

#' A helper function that takes an egor object and a list with arguments
#' to ego_design and runs svydesign()
#'
#' @param egor an [`egor`] object (possibly missing design
#'   information).
#' @param ego_design either `survey.design` object (like one
#'   constructed by [svydesign()]) or a [`list`] of arguments to
#'   [svydesign()] specifying the sampling design for the egos. If the
#'   arguments are formulas, they can refer to columns (ego
#'   attributes) of `egor`. If `survey.design`, returned unchanged.
#' @param how many parents up from the calling function should the
#'   function look for variables not in egor. If the calling function
#'   is meant to be called directly by the user, this number should be
#'   1; if it's called from a function called by the user, 2; etc..
#'
#' @noRd
.gen.ego_design <- function(egor, ego_design, depth){
#' @importFrom methods is
  if(is(ego_design, "survey.design")) return(ego_design)

  # TODO: Save space by only including the columns with the design
  # information.
  pf <- parent.frame(depth+1)
  svyenv <- new.env(parent=pf)
  assign("egor", egor, envir=svyenv)
#' @importFrom survey svydesign
  svycall <- as.call(c(call("::",as.name("survey"),as.name("svydesign")), ego_design, list(data = as.name("egor"))))
  suppressWarnings(eval(svycall, svyenv))
}

#' Set and query the ego sampling design
#'
#' Extract, set, or update the [`svydesign`] associated with an
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
#'   [svydesign()]) or a [`list`] of arguments to [svydesign()]
#'   specifying the sampling design for the egos. If the arguments are
#'   formulas, they can refer to columns (ego attributes) of `x`.
#'
#' @note This can be useful for adjusting or reinitializing the ego
#'   design information after the underlying ego attributes had been
#'   modified.
#' @export
`ego_design<-.egor` <- function(x, ..., value){
  attr(x, "ego_design") <- .gen.ego_design(x, value, 1)
  x
}
