# Density

#' Calculate the relationship density in ego-centered networks
#'
#' This function uses an \code{egor} object and calculates the density of all 
#' the ego-centered networks listed in the 'egor' object. Instead of an 
#' \code{egor} object, alter and alter-alter data can be provided as \code{lists}
#' or \code{data.frames}. 
#' @param x Either an \code{egor} object or a \code{data.frame/ list} containg alter-alter
#' relations.
#' @param aaties \code{data.frame/ list} containg alter attributes 
#' (not needed if x is an \code{egor} object).
#' @param egoID Name of ego ID variable. Only needs to be specified if alter
#' and alter-alter data is provided in global data.frames.
#' @param max.netsize Optional parameter. Constant value used if the
#' number of alters whose relations were collected is limited.
#' @param directed logical indicating if the alter-alter relation data/ edges 
#' are directed or undirected.
#' @param weight \code{Character} naming a variable containing the weight values
#' of relations. Weights should range from 0 to 1.
#' @return returns a \code{vector} of network density values.
#' @keywords ego-centric network analysis
#' @examples 
#' data("egor32")
#' egor_density(egor32)
#' @export
ego_density <- function (x, ...) {
  UseMethod("ego_density", x)
}


#' @rdname ego_density
#' @export
ego_density.list <- function(x, aaties, weight = NULL, max.netsize = NULL, directed = FALSE) {
  if(!is.null(weight)) {
    dyaden_real <- plyr::ldply(aaties, .fun = function(x) sum(x[[weight]], na.rm = T))
    
  } else {
    dyaden_real <- plyr::ldply(aaties, .fun = function(x) NROW(x))
  }
  
  netsize <- unlist(lapply(x, FUN = NROW))
  
  if(!is.null(max.netsize)) {
    netsize[netsize > max.netsize] <- max.netsize
  }
  
  dyad_poss <- (netsize^2 - netsize)
  
  if(!directed) {
    dyad_poss <- dyad_poss / 2
  }
  
  density <- as.vector(dyaden_real$V1 / dyad_poss)
  density[is.infinite(density)] <- NA
  density
}


#' @rdname ego_density
#' @export
ego_density.egor <- function(x, weight = NULL, max.netsize = NULL, directed = FALSE) {
  ego_density(x = x$.alters, aaties = x$.aaties,  weight = weight, max.netsize = max.netsize, directed = directed)
}


#' @rdname ego_density
#' @export
ego_density.data.frame <- function(x, aaties, egoID = "egoID", weight = NULL, max.netsize = NULL, directed = FALSE) {
  x <- split(x, x[egoID])
  aaties <- split(aaties, aaties[egoID])
  ego_density(x = x, aaties = aaties, weight = weight, max.netsize = max.netsize, directed = directed)
}

