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
#' @return returns a \code{vector} of network density values.
#' @keywords ego-centric network analysis
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