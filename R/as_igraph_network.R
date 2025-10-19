if (getRversion() >= "2.15.1")
  utils::globalVariables(c("aatie"))


igraph_set_graph_attrs <- function(graph, attr_names, attrs) {
  purrr::reduce2(attr_names, attrs, \(graph, x, y)  igraph::set_graph_attr(graph, x, y), .init = graph)
}

network_set_graph_attrs <- function(network, attr_names, attrs) {
  purrr::reduce2(attr_names, attrs, \(network, x, y)  network::set.network.attribute(network, x, y), .init = network)
}

#' @importFrom tidyselect all_of
as_igraph_network <- function(x,
                              directed = FALSE,
                              include.ego = FALSE,
                              ego.attrs = NULL,
                              ego.alter.weights = NULL,
                              graph.attrs = ".egoID",
                              to = c("igraph", "network")) {
  
  
  # split_egor_egoid() splits an egor object into a list of egor 
  # objects with one object for each ego
  #  --> helps to handle no alter/no aatie networks
  split_egor_egoid <- function(x) {
    lapply(1:nrow(x$ego), \(y) slice(x, y))
  }
  
  # ID vars should always be characters; this should maybe be enforced at creation
  # of egor objects
  all_ID_as_char <- function(x) {
   x |> 
      activate(ego) |> 
      mutate(.egoID = as.character(.egoID)) |> 
      activate(alter) |> 
      mutate(across(c(.egoID, .altID), as.character)) |> 
      activate(aatie) |> 
      mutate(across(c(.egoID, .srcID, .tgtID), as.character)) 
  }
  
  egor_split <- 
    x |> 
    all_ID_as_char() |> 
    activate(ego) |> 
    split_egor_egoid()
  
  graph_attrs_l <- 
    egor_split |> 
    lapply(select, tidyselect::all_of(c(".egoID", graph.attrs))) |> 
    lapply(as_tibble)
  
  vertices_l <-
    egor_split |> 
    lapply(\(x) x$alter) |> 
    lapply(relocate, .egoID, .after = .altID)
  
  edges_l <- 
    egor_split |> 
    lapply(\(x) x$aatie) |> 
    lapply(relocate, .egoID, .after = .tgtID)
  
  if (include.ego | !is.null(ego.attrs)) {
    ego_alter_edges <-
      egor_split |>
      lapply(activate, alter) |> 
      lapply(as_tibble) |> 
      lapply(select, .egoID, .altID, weight = all_of(ego.alter.weights)) |>
      lapply(mutate, .srcID = "ego") |>
      lapply(rename, .tgtID = .altID) |> 
      lapply(relocate, .egoID, .after = .srcID)
    
    edges_l <- 
      purrr::map2(edges_l,
           ego_alter_edges,
           bind_rows)
    
    egos <-
      egor_split |> 
      lapply(activate, ego) |>
      lapply(as_tibble) |>
      lapply(mutate, .altID = "ego")  |>
      lapply(select, .egoID, .altID, all_of(ego.attrs))

    vertices_l <- 
      purrr::map2(vertices_l,
                  egos,
                  bind_rows)
  }
  
  if (to == "igraph") {
    purrr::map2(vertices_l,
                edges_l,
                \(x, y) igraph::graph_from_data_frame(y, vertices = x)) |> 
      purrr::map2(graph_attrs_l, \(x, y) igraph_set_graph_attrs(x, names(y), y))
  } else if (to == "network") {
    purrr::map2(vertices_l, 
                edges_l, 
                \(x, y) network::as.network(y, vertices = x)) |> 
      purrr::map2(graph_attrs_l, \(x, y) network_set_graph_attrs(x, names(y), y))
  }
  
}

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
#' igraph objects. This is ignored, when `include.ego = FALSE` (default).
#' @param ego.alter.weights  Vector of names (character) or indices (numeric) of
#' alter variables that should be carried over to the the
#' network/ igraph objects, as edge attributes of the ego-alter relations.
#' This is ignored, when `include.ego = FALSE`` (default).
#' @param graph.attrs Vector of names (character) or indices (numeric) of
#' ego variables that are supposed to be carried over to the igraph object
#' as graph attributes or the network object as network attributes. By
#' default `.egoID` is carried over.
#' @details The names of the variables specified in ego.attr and ego.alter.attr
#' need to be the same as the names of corresponding alter attributes,
#' in order for those variables to be merged successfully in the resulting
#' network/ igraph object (see example).
#' @examples
#' e <- make_egor(3, 22)
#' as_igraph(e)
#' @export
as_igraph <- function(x,
                      directed = FALSE,
                      include.ego = FALSE,
                      ego.attrs = NULL,
                      ego.alter.weights = NULL,
                      graph.attrs = ".egoID") {
  require_igraph()
  UseMethod("as_igraph", x)
}

#' @method as_igraph egor
#' @export
as_igraph.egor <- function(x,
                           directed = FALSE,
                           include.ego = FALSE,
                           ego.attrs = NULL,
                           ego.alter.weights = NULL,
                           graph.attrs = ".egoID") {
  require_igraph()
  as_igraph_network(
    x = x,
    directed = directed,
    include.ego = include.ego,
    ego.attrs = ego.attrs,
    ego.alter.weights = ego.alter.weights,
    graph.attrs = graph.attrs,
    to = "igraph"
  )
}

#' @rdname as_igraph
#' @exportS3Method igraph::as.igraph egor
#' @method as.igraph egor
as.igraph.egor <- as_igraph

#' @rdname as_igraph
#' @export
as_network <- function(x,
                       directed = FALSE,
                       include.ego = FALSE,
                       ego.attrs = NULL,
                       ego.alter.weights = NULL,
                       graph.attrs = ".egoID") {
  require_network()
  as_igraph_network(
    x = x,
    directed = directed,
    include.ego = include.ego,
    ego.attrs = ego.attrs,
    ego.alter.weights = ego.alter.weights,
    graph.attrs = graph.attrs,
    to = "network"
  )
}

#' @rdname as_igraph
#' @exportS3Method network::as.network egor
#' @method as.network egor
as.network.egor <- as_network
