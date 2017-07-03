#' Set and query the alter nomination design
#'
#' Extract, set, or update the alter nomination design associated with
#' an egocentric dataset.
#'
#' @param x an [`egor`] object.
#' @template meth_dots
#' @docType methods
#' @export
alter.design <- function(x, ...) UseMethod("alter.design")

#' @rdname alter.design
#' @param which name of the alter design setting to query or replace
#' @export
alter.design.egor <- function(x, which, ...){
  if(missing(which)) attr(x, "alter.design")
  else attr(x, "alter.design")[[which]]
}

#' @rdname alter.design
#' @export
`alter.design<-` <- function(x, ..., value) UseMethod("alter.design<-")

#' @rdname alter.design
#' @param value if `which` is specified, the new value of the
#'   attribute; if not, a named list of settings that replace their
#'   old values.
#' @export
`alter.design<-.egor` <- function(x, which, value){
  if(missing(which)){
    if(!is.list(value)) stop("Specify which settings to change.")
#' @importFrom utils modifyList
    attr(x, "alter.design") <- modifyList(attr(x, "alter.design"), value)
  }else{
    attr(x, "alter.design")[[which]] <- value
  }
  x
}

