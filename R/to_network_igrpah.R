#' Convert egor object to network or igraph objects
#' 
#' These functions turns an egor object into a list of network or igraph objects. 
#' By default ego itself is not included in the created objects, but 
#' there is a parameter that allow for including ego. By specifying variable 
#' names on the  ego level, that correspond to alter variables and
#' attributes of ego-alter relations, matching ego and alter variables can be
#' included (see details).
#' 
#' @param object An egor object.
#' @param include.ego \code{Logical.} Should ego be included?
#' @param ego.attr Vector of names (character) or indices (numeric) of ego 
#' variables that should be carried over to  the network/ 
#' igraph objects.
#' @param ego.alter.attr  Vector of names (character) or indices (numeric) of 
#' alter variables that should be carried over to the the 
#' network/ igraph objects, as edge attributes of the ego-alter relations.
#' @details The names of the variables specified in ego.attr and ego.alter.attr 
#' need to be the same as the names of corresponding alter attributes,
#' in order for those variables to be complete in the resulting network/ igraph 
#' object (see example).
#' @name convert_egor
NULL

#' @describeIn convert_egor Creates a lis of statnets network objects.
#' @export
to_network <- function(object, directed = F, include.ego = F, ego.attrs = NULL, ego.alter.weights = NULL) {
  
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
                                                    stringsAsFactors = F))
      
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
                                                      stringsAsFactors = F))
      obj
    }
    
    object <- as_tibble(object)
    object <- do.call(rbind, lapply(split(object, rownames(object)), FUN = add_ego_network))
  }
  

  network.data.frame <- function(aaties, alts) {
    n <- network::network(matrix(c(aaties[[1]], aaties[[2]]), ncol = 2), directed = directed)
    for(i in 1:NCOL(alts)) 
      n <- network::set.vertex.attribute(n, names(alts)[i], as.character(alts[[i]]))
    if(NCOL(aaties) > 2)
      for(i in 3:(NCOL(aaties)))
        n <- network::set.edge.attribute(n, names(aaties)[i], aaties[[i]])
    n
  }
  networks <- mapply(FUN = network.data.frame,
                     object$.aaties,
                     object$.alts,
                     SIMPLIFY = F)


  # Return
  networks
}

#' @describeIn convert_egor Creates a list of igraph objects.
#' @export
to_igraph <- function(object, 
                      directed = F, 
                      include.ego = F, 
                      ego.attrs = NULL, 
                      ego.alter.weights = NULL) {
  
  # Create igraphs
  igraphs <- mapply(FUN = function(x,y) igraph::graph.data.frame(d = x, 
                                                        vertices = y, 
                                                        directed = directed), 
           object$.aaties, 
           object$.alts, 
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
      alter_names <- names(V(graph))[-length(V(graph))]
      ego_edges <- Reduce(c,sapply(alter_names, 
                                   FUN = function(x)  c("ego",x), 
                                   simplify = T))
      graph <- add_edges(graph, 
                         ego_edges, 
                         attr = alts[ego.alter.weights])
      # Return
      graph
    }
    
    igraphs <- mapply(FUN = add_ego_igraph, 
                      igraphs, 
                      object$.alts, 
                      object$.aaties, 
                      split(tibble::as_tibble(object)[ego.attrs], 
                            rownames(object)), SIMPLIFY = F)
  }
  # Return
  igraphs
}




