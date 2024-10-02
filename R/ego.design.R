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
#' to ego_design and runs [srvyr::as_survey_design()].
#'
#' @param egor an [`egor`] object (possibly missing design
#'   information).
#' @templateVar ego_design_name ego_design
#' @template ego_design
#' @param pos where the call to `as_survey_design`.
#'
#' @return If `ego_design` is a `list`, [`ego`] as a [`tbl_svy`]. If
#'   `NULL`, design information is not set or is cleared if present,
#'   returning a `tbl_df`.
#'
#' @noRd
.gen.ego_design <- function(egor, ego_design, pos=-1L){
  cl <- rlang::caller_env()
  tryCatch(force(ego_design),
           error = function(e) rlang::abort(c(conditionMessage(e),
                                              i = paste0("Did you pass ego design variable names unquoted and wrap them in ",
                                                         sQuote('list()'), " rather than ", sQuote('alist()'), "?")),
                                            use_cli_format = TRUE, call = cl))

  egos <- if(is(egor, "nested_egor")) egor else egor$ego
  if(is.null(ego_design)) return(as_tibble(egos))

  envir <- as.environment(pos)
#' @importFrom srvyr as_survey_design
  do.call(as_survey_design, c(list(egos), ego_design), envir=envir)
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
#' @templateVar ego_design_name value
#' @template ego_design
#'
#' @note This can be useful for adjusting or re-initializing the ego
#'   design information after the underlying ego attributes had been
#'   modified.
#'
#' @examples
#' data(egor32)
#'
#' ego_design(egor32)
#'
#' ego_design(egor32) <- alist(strata = sex)
#'
#' ego_design(egor32)
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
