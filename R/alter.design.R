#' Set and query the alter nomination design
#'
#' Extract, set, or update the alter nomination design associated with
#' an ego-centered dataset.
#'
#' @param x an [`egor`] object.
#' @template meth_dots
#' @docType methods
#' @export
alter_design <- function(x, ...) UseMethod("alter_design")

#' @rdname alter_design
#' @param which name of the alter design setting to query or replace
#' @export
alter_design.egor <- function(x, which, ...){
  if(missing(which)) attr(x, "alter_design")
  else attr(x, "alter_design")[[which]]
}

#' @rdname alter_design
#' @export
`alter_design<-` <- function(x, ..., value) UseMethod("alter_design<-")

#' @rdname alter_design
#' @param value if `which` is specified, the new value of the
#'   attribute; if not, a named list of settings that replace their
#'   old values.
#' @export
`alter_design<-.egor` <- function(x, which, ..., value){
  if(missing(which)){
    if(!is.list(value)) stop("Specify which settings to change.")
#' @importFrom utils modifyList
    attr(x, "alter_design") <- modifyList(if(is.null(alter_design(x))) list() else alter_design(x), value)
  }else{
    attr(x, "alter_design")[[which]] <- value
  }
  x
}

