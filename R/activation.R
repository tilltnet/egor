#' Activate ego, alter or alter-alter tie data llevel of an egor object
#'
#' This function activates one of the data levels of an egor object, so that the dplyr verbs know which level to execute on.
#' @param obj The \code{egor} object.
#' @param what \code{Character} naming the level to activate, this can be "ego", "alter" or "aatie".
#' @keywords ego-centered network
#' @export
activate <- function(obj, what) {
  what <- match.arg(what, UNITS)
  attr(obj, "active") <- what
  obj
}