if (getRversion() >= "2.15.1") utils::globalVariables(c(".tmp_row_id"))

# egor conversions

#' Convert `egor` object to `network` or `igraph` objects
#' 
#' These functions convert an `egor` object into a list of `network` or `igraph` objects. 
#' By default ego itself is not included in the created objects, there is 
#' a parameter (**include.egor**) that allows for including ego.
#' 
#' @param x An `egor` object.
#' @param include.ego \code{Logical.} Should ego be included?
#' @param directed Logical, indicating if alter-alter relations are directed.
#' @param ego.attrs Vector of names (character) or indices (numeric) of ego 
#' variables that should be carried over to  the network/ 
#' igraph objects.
#' @param ego.alter.weights  Vector of names (character) or indices (numeric) of 
#' alter variables that should be carried over to the the 
#' network/ igraph objects, as edge attributes of the ego-alter relations.
#' @details The names of the variables specified in ego.attr and ego.alter.attr 
#' need to be the same as the names of corresponding alter attributes,
#' in order for those variables to be complete in the resulting network/ igraph 
#' object (see example).
#' @export
as_igraph <- function(x, 
                      directed = FALSE, 
                      include.ego = FALSE, 
                      ego.attrs = NULL, 
                      ego.alter.weights = NULL) {
  UseMethod("as_igraph", x)
}
  
#' @method as_igraph egor
#' @export
as_igraph.egor <- function(x, 
                           directed = FALSE, 
                           include.ego = FALSE, 
                           ego.attrs = NULL, 
                           ego.alter.weights = NULL) {
  x <- as_nested_egor(x)
  as_igraph(x, directed, include.ego, ego.attrs, ego.alter.weights)
}

#' @rdname as_igraph
#' @export
as_igraph.nested_egor <- function(x, 
                      directed = FALSE, 
                      include.ego = FALSE, 
                      ego.attrs = NULL, 
                      ego.alter.weights = NULL) {

  # Create igraphs
  igraphs <- mapply(FUN = function(x,y) igraph::graph.data.frame(d = x, 
                                                                 vertices = y, 
                                                                 directed = directed), 
                    x$.aaties, 
                    x$.alts,
                    SIMPLIFY = FALSE)
  
  # Include Ego
  if (include.ego) {
    if (is.null(ego.attrs)) ego.attrs <- 0
    if (is.null(ego.alter.weights)) ego.alter.weights <- 0
    add_ego_igraph <- function(graph, alts, aaties, ego.attrs.df) {
      # Add ego vertex
      graph <- igraph::add_vertices(graph = graph, 
                                    nv = 1, 
                                    name  = "ego", 
                                    attr = data.frame(
                                      lapply(ego.attrs.df, as.character), 
                                      stringsAsFactors=FALSE))
      
      # Add ego alter edges
      alter_names <- names(igraph::V(graph))[-length(igraph::V(graph))]
      ego_edges <- Reduce(c,sapply(alter_names, 
                                   FUN = function(x)  c("ego",x), 
                                   simplify = TRUE))
      graph <- igraph::add_edges(graph, 
                                 ego_edges, 
                                 attr = alts[ego.alter.weights])
      # Return
      graph
    }
    
    igraphs <- mapply(FUN = add_ego_igraph, 
                      igraphs, 
                      x$.alts, 
                      x$.aaties, 
                      split(x[ego.attrs], 
                            rownames(x)), 
                      SIMPLIFY = FALSE)
  }
  # Return
  igraphs
}

#' @rdname as_igraph
#' @importFrom igraph as.igraph
#' @export
as.igraph.egor <- as_igraph

#' Creates a list of statnet's network objects, from an
#' `egor` object.
#' @param x An egor Object.
#' @param directed Logical.
#' @param include.ego Logical.
#' @param ego.attrs Names of ego variables.
#' @param ego.alter.weights Name of ego alter weight variable.
#' @export
#' @importFrom network network 
#' @importFrom network network.initialize
#' @importFrom network set.vertex.attribute 
#' @importFrom network set.edge.attribute
as_network <- function(x, 
                       directed = FALSE, 
                       include.ego = FALSE, 
                       ego.attrs = NULL, 
                       ego.alter.weights = NULL) {
  alters_l <- split(x$alter, factor(x$alter$.egoID, levels = x$ego$.egoID))
  aaties_l <- split(x$aatie, factor(x$aatie$.egoID, levels = x$ego$.egoID))
  x <- x$ego
  x$.aaties <- aaties_l
  x$.alts <- alters_l
  # Incldude Ego
  if (include.ego) {
    if (is.null(ego.attrs)) ego.attrs <- 0

    # Add ego vertex
    add_ego_network <- function(obj) {
      #obj$.alts[[1]]$.altID <- as.character(obj$.alts[[1]]$.altID)
      obj$.alts[[1]] <- mutate_all(obj$.alts[[1]], as.character)
      alt_rows <- mutate_all(obj[ego.attrs], as.character)
      obj$.alts[[1]] <- dplyr::bind_rows(obj$.alts[[1]], 
                                         data.frame(.altID = "ego", 
                                                    alt_rows,
                                                    stringsAsFactors = FALSE))
      
      # Add ego alter edges
      alter_names <- as.character(obj$.alts[[1]]$.altID)
      len <- length(alter_names)
      alter_names <- alter_names[-len]
      obj$.aaties[[1]]$.srcID <- as.character(obj$.aaties[[1]]$.srcID)
      obj$.aaties[[1]]$.tgtID <- as.character(obj$.aaties[[1]]$.tgtID)

      if (!is.null(ego.alter.weights)) {
        ea_weights <- obj$.alts[[1]][ego.alter.weights][1:(len-1),]
        ea_weights <- mutate_all(tibble(ea_weights = ea_weights), as.character)
        suppressWarnings(
          obj$.aaties[[1]] <- dplyr::mutate_at(
            obj$.aaties[[1]], 
            dplyr::vars(dplyr::one_of(ego.alter.weights)),
            as.character)
        )
      }
      ea_rows <- data.frame(.srcID = "ego", 
                            .tgtID = alter_names,
                            stringsAsFactors = FALSE)
      if (!is.null(ego.alter.weights)) ea_rows <- dplyr::bind_cols(ea_rows, ea_weights)
        
      
      obj$.aaties[[1]] <- dplyr::bind_rows(obj$.aaties[[1]], ea_rows)
      obj
    }
    
    
    x <- do.call(rbind, lapply(split(x, x$.egoID), FUN = add_ego_network))
  }
  
  
  network.data.frame <- function(aaties, alts) {
    if (nrow(aaties) == 0) {
      n <- network.initialize(0)
      } else {
        all_alt_IDs <- alts$.altID
        mt <- matrix(0, 
                   (la <- length(all_alt_IDs)), 
                   la,
                   dimnames = list(all_alt_IDs, all_alt_IDs))
        for (i in all_alt_IDs) {
          mt[colnames(mt) == i, colnames(mt) %in% aaties[aaties$.srcID == i ,]$.tgtID] <- 1
          if (!directed) mt[colnames(mt) %in% aaties[aaties$.srcID == i ,]$.tgtID, colnames(mt) == i] <- 1
        }
        n <- network(mt, directed = FALSE)
    }
    for (i in 1:NCOL(alts)) 
      n <- network::set.vertex.attribute(n, names(alts)[i], as.character(alts[[i]]))
    if (NCOL(aaties) > 2)
      for (i in 3:(NCOL(aaties)))
        n <- network::set.edge.attribute(n, names(aaties)[i], aaties[[i]])
      n
  }
  networks <- mapply(FUN = network.data.frame,
                     aaties_l,
                     alters_l,
                     SIMPLIFY = FALSE)
  
  
  # Return
  networks
}

#' @rdname as_network
#' @importFrom network as.network
#' @export
as.network.egor <- as_network

#' Create global alters and alter-alter relations dataframes from an `egor` object
#' 
#' Provided an egor-object, these functions create a 'global' \code{data.frame},
#' containing alter attributes, or alter-alter relations. The resulting dataframes
#' are useful for advanced analysis procedures, e.g. multi-level regressions.
#' 
#' @param object An `egor` object.
#' a new variable with the specified name is created.
#' @param include.ego.vars Logical, specifying if ego variables should be included in the result.
#' @param include.alt.vars Logical, specifying if alter variables should be included in the result.
#' @examples 
#' # Load example data
#' data(egor32)
#' 
#' # Create global alters dataframes
#' as_alters_df(egor32)
#' 
#' # Create global alter-alter relaions dataframes
#' as_aaties_df(egor32)
#' 
#' # ... adding alter variables
#' as_aaties_df(egor32, include.alt.vars = TRUE)
#' @export
as_alters_df <- function(object, include.ego.vars = FALSE) {
  object <- activate(object, "alter")
  as_tibble.egor(object, include.ego.vars = include.ego.vars)
}

#' @rdname as_alters_df
#' @export
#' @importFrom dplyr left_join
#' @importFrom purrr map_lgl
as_aaties_df <- function(object, 
                       include.ego.vars = FALSE,
                       include.alt.vars = FALSE) {
  object <- activate(object, "aatie")
  as_tibble.egor(object, include.ego.vars = include.ego.vars)
}
