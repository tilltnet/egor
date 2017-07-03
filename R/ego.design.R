#' [weights.egor()] extracts the (relative) sampling weights of each
#' ego in the dataset.
#'
#' @param object an [`egor`] object.
#' @template meth_dots
#'
#' @seealso [survey:::weights.survey.design()]
#'
#' @export
#' @importFrom stats weights
weights.egor <- function(object, ...) {
  weights(attr(object,"ego.design"), ...)
}

#' A helper function that takes an egor object and a list with arguments
#' to ego.design and runs svydesign()
#'
#' @param egor an [`egor`] object (possibly missing design
#'   information).
#' @param ego.design either `survey.design` object (like one
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
.gen.ego.design <- function(egor, ego.design, depth){
#' @importFrom methods is
  if(is(ego.design, "survey.design")) return(ego.design)

  # TODO: Save space by only including the columns with the design
  # information.
  pf <- parent.frame(depth+1)
  svyenv <- new.env(parent=pf)
  assign("egor", egor, envir=svyenv)
#' @importFrom survey svydesign
  svycall <- as.call(c(call("::",as.name("survey"),as.name("svydesign")), ego.design, list(data = as.name("egor"))))
  eval(svycall, svyenv)
}

#' Set and query the ego sampling design
#'
#' Extract, set, or update the [`svydesign`] associated with an
#' egocentric dataset.
#'
#' @param x an [`egor`] object.
#' @template meth_dots
#' @docType methods
#' @export
ego.design <- function(x, ...) UseMethod("ego.design")

#' @rdname ego.design
#' @export
ego.design.egor <- function(x, ...) attr(x, "ego.design")

#' @rdname ego.design
#' @export
`ego.design<-` <- function(x, ..., value) UseMethod("ego.design<-")

#' @rdname ego.design
#' @param value either `survey.design` object (like one constructed by
#'   [svydesign()]) or a [`list`] of arguments to [svydesign()]
#'   specifying the sampling design for the egos. If the arguments are
#'   formulas, they can refer to columns (ego attributes) of `x`.
#'
#' @note This can be useful for adjusting or reinitializing the ego
#'   design information after the underlying ego attributes had been
#'   modified.
#' @export
`ego.design<-.egor` <- function(x, value){
  attr(x, "ego.design") <- .gen.ego.design(x, value, 1)
  x
}
