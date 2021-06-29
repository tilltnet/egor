#' Activate ego, alter or alter-alter tie data level of an egor dataset
#'
#' This function activates one of the data levels of an egor dataset, so that the dplyr verbs know which level to execute on.
#' @param .data The \code{egor} dataset.
#' @param what \code{Character} naming the level to activate, this can be "ego", "alter" or "aatie".
#' @importFrom tidygraph activate
#' @keywords ego-centered network
#' @export
#' @method activate egor
#' @examples 
#' e <- make_egor(5,50)
#' e %>% 
#'    activate("aatie") %>% 
#'    mutate(weight2 = 2 + weight) %>% 
#'    activate("alter") %>% 
#'    mutate(age.years = age.years^3)
activate.egor <- function(.data, what) {
  what <- as.character(substitute(what))
  what <- match.arg(what, UNITS)
  attr(.data, "active") <- what
  .data
}

#' @export
tidygraph::activate