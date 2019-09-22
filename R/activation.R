#' Activate ego, alter or alter-alter tie data llevel of an egor .dataect
#'
#' This function activates one of the data levels of an egor .dataect, so that the dplyr verbs know which level to execute on.
#' @param .data The \code{egor} .dataect.
#' @param what \code{Character} naming the level to activate, this can be "ego", "alter" or "aatie".
#' @importFrom tidygraph activate
#' @keywords ego-centered network
#' @export
#' @method activate egor
activate.egor <- function(.data, what) {
  what <- as.character(substitute(what))
  what <- match.arg(what, UNITS)
  attr(.data, "active") <- what
  .data
}

#' @export
tidygraph::activate