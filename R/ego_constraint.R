#' Calculate Burt constraint for the egos of ego-centered networks
#'
#' This calculates Burt's *network constraint* for all egos in an egor object.
#' It iterates over each network and applies [igraph::constraint]. A
#' weight variable can be specified.
#'
#' @template object
#' @param weights `Character`, naming the alter-alter tie weight variable.
#' @param ego.alter.weights `Character`, naming the ego-alter weight tie weight
#' variable. This defaults to the same value as `weights`, only specify if the
#' name of the ego.alter.weights is different from `weights`.
#' @details The calculation of weighted network constraint only works, if
#' the alter-alter tie weights are complemented by a alter level variable 
#' specifying the same weight for the ego-alter ties.
#' @return `Numeric` vector with a constraint value for each ego.
#' @references
#' Burt, R. (2004). Structural holes and good ideas. *American Journal of Sociology*, (110), 349–399.
#' @examples 
#' data(egor32)
#' ego_constraint(egor32)
#' @export
ego_constraint <-
  function(object,
           weights = NULL,
           ego.alter.weights = weights) {
    graphs <-
      as_igraph(object,
                include.ego = TRUE,
                ego.alter.weights = ego.alter.weights)
    
    res <- purrr::map_dbl(graphs,
            ~ igraph::constraint(
              .,
              weights = if (is.null(weights))
                rep(1, length(igraph::E(.)))
              else
                get.edge.attribute(., weights),
              nodes = igraph::V(.)[igraph::V(.)$name == "ego"]
            ))
    if(has_ego_design(object)) 
      res <- tibble(.egoID = object$ego$variables$.egoID, constraint = res)
    else res <- tibble(.egoID = object$ego$.egoID, constraint = res)
    return_results(object, res)
  }
