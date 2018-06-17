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
#' @name convert_egor
NULL

#' @describeIn convert_egor Creates a list of igraph objects from an
#' `egor` object.
#' @export
as_igraph <- function(x, 
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
  if(include.ego) {
    if(is.null(ego.attrs)) ego.attrs <- 0
    if(is.null(ego.alter.weights)) ego.alter.weights <- 0
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
                      split(tibble::as_tibble(x)[ego.attrs], 
                            rownames(x)), SIMPLIFY = FALSE)
  }
  # Return
  igraphs
}

#' @rdname convert_egor
#' @importFrom igraph as.igraph
#' @export
as.igraph.egor <- as_igraph

#' @describeIn convert_egor Creates a list of statnet's network objects, from an
#' `egor` object.
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
  
  # Incldude Ego
  if(include.ego) {
    if(is.null(ego.attrs)) ego.attrs <- 0
    if(is.null(ego.alter.weights)) ego.alter.weights <- 0
    
    # Add ego vertex
    add_ego_network <- function(obj) {
      obj$.alts[[1]]$.altID <- as.character(obj$.alts[[1]]$.altID)
      obj$.alts[[1]] <- dplyr::bind_rows(obj$.alts[[1]], 
                                         data.frame(.altID = "ego", 
                                                    obj[ego.attrs],
                                                    stringsAsFactors = FALSE))
      
      # Add ego alter edges
      alter_names <- as.character(obj$.alts[[1]]$.altID)
      len <- length(alter_names)
      alter_names <- alter_names[-len]
      obj$.aaties[[1]]$.srcID <- as.character(obj$.aaties[[1]]$.srcID)
      obj$.aaties[[1]]$.tgtID <- as.character(obj$.aaties[[1]]$.tgtID)
      obj$.aaties[[1]] <- dplyr::bind_rows(obj$.aaties[[1]], 
                                           data.frame(.srcID = "ego", 
                                                      .tgtID = alter_names,
                                                      obj$.alts[[1]][ego.alter.weights][-len,],
                                                      stringsAsFactors = FALSE))
      obj
    }
    
    x <- as_tibble(x)
    x <- do.call(rbind, lapply(split(x, rownames(x)), FUN = add_ego_network))
  }
  
  
  network.data.frame <- function(aaties, alts) {
    if(nrow(aaties) == 0) {
      n <- network.initialize(0)
      } else {
        all_alt_IDs <- alts$.altID
        mt <- matrix(0, 
                   (la<-length(all_alt_IDs)), 
                   la,
                   dimnames = list(all_alt_IDs, all_alt_IDs))
        for(i in all_alt_IDs) {
          mt[colnames(mt) == i, colnames(mt) %in% aaties[aaties$.srcID == i ,]$.tgtID] <- 1
          if(!directed) mt[colnames(mt) %in% aaties[aaties$.srcID == i ,]$.tgtID, colnames(mt) == i] <- 1
        }
        n <- network(mt, directed = FALSE)
    }
    for(i in 1:NCOL(alts)) 
      n <- set.vertex.attribute(n, names(alts)[i], as.character(alts[[i]]))
    if(NCOL(aaties) > 2)
      for(i in 3:(NCOL(aaties)))
        n <- set.edge.attribute(n, names(aaties)[i], aaties[[i]])
      n
  }
  networks <- mapply(FUN = network.data.frame,
                     x$.aaties,
                     x$.alts,
                     SIMPLIFY = FALSE)
  
  
  # Return
  networks
}

#' @rdname convert_egor
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
#' @param egoID Character, naming the variable used to identify egos. If this variable does not exist,
#' a new variable with the specified name is created.
#' @param include.ego.vars Logical, specifying if ego variables should be included in the result.
#' @param include.alt.vars Logical, specifying if alter variables should be included in the result.
#' @param aatie_vars Character vector, specifying the names of the source and target columns.
#' @examples 
#' # Load example data
#' data(egor32)
#' 
#' # Create global alters dataframes
#' # ... creating a new egoID
#' as_alts_df(egor32)
#' 
#' # ... without creating a new egoID
#' as_alts_df(egor32, FALSE)
#' 
#' # ... keeping ego variables
#' as_alts_df(egor32, FALSE, TRUE) 
#' 
#' # Create global alter-alter relaions dataframes
#' as_aaties_df(egor32)
#' 
#' # ... adding alter variables
#' as_aaties_df(egor32, include.alt.vars = TRUE)
#' @export
as_alts_df <- function(object, egoID = "egoID", include.ego.vars = FALSE) {
  alts_names <- names(object$.alts[[1]])
  
  # Check if egoID is present in ego vars; yes: delete  (possibly existing) 
  # egoID from alters list; no: check if egoID does not exisit in alter list; 
  # yes: create new egoID with name specfied in egoID param
  if(egoID %in% names(object)) {
    object$.alts <- lapply(object$.alts, FUN = function(x) {
      x[, !names(x) %in% egoID]
    }) 
    alts_names <- names(object$.alts[[1]])
  } else if(!egoID %in% alts_names) {
    object[egoID] <- 1:NROW(object)
  }
    
  object <- tibble::as_tibble(object)
  ego_names <- names(object)[!names(object) %in% c(".alts", egoID)]
  
  object <- tidyr::unnest_(object,  ".alts")
  if(!include.ego.vars) {
    object <- object[, !names(object) %in% c(ego_names)]
    names(object)[2:NCOL(object)] <- alts_names
    object
  } else {
    object
  }
}

#' @rdname as_alts_df
#' @export
#' @importFrom dplyr left_join
#' @importFrom purrr map_lgl
as_aaties_df <- function(object, 
                       egoID = "egoID", 
                       include.ego.vars = FALSE,
                       include.alt.vars = FALSE,
                       aatie_vars = c(".srcID", ".tgtID")) {
  aaties_names <- names(object$.aaties[[1]])
  
  # Check if egoID is present in ego vars; yes: delete (possibly existing) 
  # egoID from aaties list; no: check if egoID does not exisit in aaties list; 
  # yes: create new egoID with name specfied in egoID param
  if(egoID %in% names(object)) {
    object$.aaties <- lapply(object$.aaties, FUN = function(x) {
      x[, !names(x) %in% egoID]
    }) 
    aaties_names <- names(object$.aaties[[1]])
  } else if(!egoID %in% aaties_names) {
    object[egoID] <- 1:NROW(object)
  }

  result <- tibble::as_tibble(object)
  ego_names <- names(result)[!names(result) %in% c(".alts", ".aaties", egoID)]
  
  result <- dplyr::filter(result, !purrr::map_lgl(result$.aaties, is.null))
  result <- tidyr::unnest_(result, ".aaties")
  
  if(include.alt.vars) {
    .alts <- as_alts_df(object, egoID = egoID)
    
    alt_names <- names(.alts)
    alt_names_src <- paste0("src_", alt_names)
    alt_names_src[1:2] <- c(egoID, ".altID")
    names(.alts) <- alt_names_src
    result <- left_join(result, .alts, by = c(".srcID" = ".altID", egoID))
    
    names(.alts) <- alt_names
    alt_names_tgt <- paste0("tgt_", alt_names)
    alt_names_tgt[1:2] <- c(egoID, ".altID")
    names(.alts) <- alt_names_tgt
    result <- left_join(result, .alts, by = c(".tgtID" = ".altID", egoID))
  }
  
  if(!include.ego.vars)
    result[, !names(result) %in% ego_names]
  else
    result
}
