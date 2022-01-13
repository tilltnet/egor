# Density

#' Calculate the relationship density in ego-centered networks
#'
#' This function uses an \code{egor} object and calculates the density of all
#' the ego-centered networks listed in the 'egor' object. Instead of an
#' \code{egor} object, alter and alter-alter data can be provided as \code{lists}
#' or \code{data.frames}.
#' @template object
#' @param max.netsize Optional parameter. Constant value used if the
#' number of alters whose relations were collected is limited.
#' @param directed logical indicating if the alter-alter relation data/ edges
#' are directed or un-directed.
#' @param weight \code{Character} naming a variable containing the weight values
#' of relations. Weights should range from 0 to 1.
#' @template meth_dots
#' @return returns a \code{vector} of network density values.
#' @keywords ego-centered network analysis
#' @examples
#' data("egor32")
#' ego_density(egor32)
#' @export
ego_density <- function(object, ...) {
  UseMethod("ego_density", object)
}

#' @rdname ego_density
#' @export
ego_density.egor <-
  function(object,
           weight = NULL,
           max.netsize = NULL,
           directed = FALSE,
           ...) {
    aatie_l <- aaties_by_ego(object)
    if (!is.null(weight)) {
      dyaden_real <- map_dbl(aatie_l, function(x)
        sum(x[[weight]]))
      
    } else {
      dyaden_real <- purrr::map_dbl(aatie_l, nrow)
    }
    
    
    alter_l <- alters_by_ego(object)
    netsize <- purrr::map_dbl(alter_l, nrow)
    
    if (!is.null(max.netsize)) {
      netsize[netsize > max.netsize] <- max.netsize
    }
    
    dyad_poss <- (netsize ^ 2 - netsize)
    
    if (!directed) {
      dyad_poss <- dyad_poss / 2
    }
    
    density <- as.vector(dyaden_real / dyad_poss)
    names(density) <- names(alter_l)
    density[is.infinite(density)] <- NA
    if(has_ego_design(object)) 
      res <- tibble(.egoID = object$ego$variables$.egoID, density = density)
    else res <- tibble(.egoID = object$ego$.egoID, density = density)
    return_results(object, res)
  }
