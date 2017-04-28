# Density

#' Calculate the density for n+many ego-centered networks. 
#'
#' This function uses an 'egoR' object (list of data objects created by an 
#' egonetR import function) and calculates the density of all 
#' the ego-centered networks listed in the 'egoR' object.
#' @param egoR A list of six data objects, created by one of egonetR's import 
#' functions.
#' @param max.netsize Optional parameter. Only needed if the density information
#' was only collected for a subsample of all alteri per network.
#' @param directed logical indicating if the alter-alter relation data/ edges 
#' are directed or undirected.
#' @param weight \code{Character} naming a variable containing the weight values
#' of relations.
#' @return returns a \code{vector} of network density values.
#' @keywords ego-centric network analysis
#' @examples 
#' data("egoR32")
#' egoR.density(egoR32)
#' @export
egoR.density <- function(egoR, weight = NULL, max.netsize = NULL, directed = FALSE) {

  if(!is.null(weight)) {
    dyaden_real <- plyr::ldply(egoR[["edges"]], .fun = function(x) sum(x[[weight]], na.rm = T))[[2]]
    
  } else {
    dyaden_real <- plyr::ldply(egoR[["edges"]], .fun = function(x) NROW(x))[[2]]
  }
  
  netsize <- egoR$results$netsize
  if(!is.null(max.netsize)) {
    netsize[netsize > max.netsize] <- max.netsize
  }
  
  dyad_poss <- (netsize^2-netsize)

  if(!directed) {
    dyad_poss <- dyad_poss/2
  }

  density <- as.vector(dyaden_real / dyad_poss)
  density
}

#' Average Alteri Relation Count
#' 
#' @template egoR
#' @template netsize
#' @seealso \code{\link{egoR.density}}
#' data("egoR32")
#' egoR.avg.rel(egoR32)
#' @export
egoR.avg.rel <- function(egoR, netsize = NULL) {
  if(is.null(netsize)) netsize <- egoR$results$netsize
  calc_avg_rel <- function(e_list, size) {
    sum(table(c(e_list[[1]], e_list[[2]])) / (size - 1))
  }
  as.vector(mapply(egoR$edges, netsize, FUN = calc_avg_rel, SIMPLIFY = T))
}

