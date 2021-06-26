if (getRversion() >= "2.15.1")
  utils::globalVariables(c("from", "to", "name", "ego_id"))


#' @rdname egor
#' @param x an object to be coerced to [`egor`].
#' @export
as.egor <- function(x, ...)
  UseMethod("as.egor")

#' @export
#' @noRd
#' @method as.egor egor
as.egor.egor <- function(x, ...)
  x

#' @export
#' @describeIn egor Can convert (legacy) `nested_egor` object to `egor` object.
#' @method as.egor nested_egor
as.egor.nested_egor <- function(x,
                                ID.vars = list(
                                  ego = ".egoID",
                                  alter = ".alterID",
                                  source = ".Source",
                                  target = ".Target"
                                ),
                                ...) {
  if (has_ego_design(x))
    x <- x$variables
  
  IDv <- modifyList(eval(formals()$ID.vars), ID.vars)
  
  if (IDv$ego %in% names(x$.alts[[1]]))
    alts <- bind_rows(x$.alts, .id = "egoID")
  else {
    alts <- select(x, IDv$ego, .alts)
    alts <- tidyr::unnest(alts, .alts)
  }
  
  if (".aaties" %in% names(x)) {
    if (IDv$ego %in% names(x$.aaties[[1]]))
      aaties <- bind_rows(x$.aaties)
    else {
      aaties <- select(x, IDv$ego, .aaties)
      aaties <- tidyr::unnest(aaties, .aaties)
    }
    egos <- select(x, -.alts, -.aaties)
    egor(
      alts,
      egos,
      aaties,
      ID.vars = list(
        ego = ".egoID",
        alter = ".altID",
        source = ".srcID",
        target = ".tgtID"
      )
    )
  } else {
    egos <- select(x, -.alts)
    egor(
      alts,
      egos,
      ID.vars = list(
        ego = ".egoID",
        alter = ".altID",
        source = ".srcID",
        target = ".tgtID"
      )
    )
  }
}

#' @rdname egor
#' @param x `list` of `igraph`/`network` objects representing ego networks.
#' @param ego_name `character` or `numeric` of length one or same length as there are networks. If the `igraph`/`network` objects don't include egos as a node, set to `NULL` (default).
#' @export
as.egor.list <-
  function(x, ego_name = NULL, ...) {
    
    if (length(ego_name) != length(x) & length(ego_name) > 1)
      stop("Length of `ego_names` does not match up with number of ego networks.")
    
    if(class(x[[1]]) == "igraph")
      as_egor_igraph(x, ego_name)
    else if (class(x[[1]]) == "network")
     as_egor_network(x, ego_name)
  }

as_egor_igraph <- 
  function(x, ego_name = NULL) {
    
    # Check if all objects are igraph
    if (!all(purrr::map_chr(x, class) == "igraph")) {
      stop(
        "At least one list element is not an `igraph` object. 
        All list elements have to be `igraph` objects for `as.egor()` to be able to convert to an `egor` object."
      )
    }
    
    graph_attrs <-
      purrr::map_dfr(x, igraph::graph_attr,
                     .id = "ego_id") %>%
      distinct(ego_id, .keep_all = TRUE)
    
    names(graph_attrs) <- gsub("\\.", "", names(graph_attrs))
    
    edges <-
      purrr::map_dfr(x,
                     igraph::as_data_frame, .id = "ego_id")
    
    alters <-
      purrr::map_dfr(x,
                     igraph::as_data_frame,
                     what = "vertices",
                     .id = "ego_id")
    
    extract_egos_and_return(graph_attrs = graph_attrs,
                            alters, edges, ego_name)
  }
  

as_egor_network <-
  function(x, ego_name = NULL) {
    
    # Check if all objects are network
    if (!all(purrr::map_chr(x, class) == "network")) {
      stop(
        "At least one list element is not a `network` object. 
        All list elements have to be `network` objects for `as.egor()` to be able to convert to an `egor` object."
      )
    }
    
    # Network Attributes
    extract_network_attributes <- function(network) {
      network_attr_names <- network::list.network.attributes(network)
      network_attr_vals <-
        purrr::map(network_attr_names, network::get.network.attribute, x = network)
      network_attr_df <- data.frame(network_attr_vals)
      network_attr_df <-
        setNames(network_attr_df, network_attr_names)
    }
    
    
    # Node Attributes
    extract_node_attributes <- function(network) {
      vertex_attr_names <- network::list.vertex.attributes(network)
      vertex_attr_vals <-
        purrr::map(vertex_attr_names, network::get.vertex.attribute, x = network)
      vertex_attr_df <- data.frame(vertex_attr_vals)
      vertex_attr_df <- setNames(vertex_attr_df, vertex_attr_names)
    }
    # Edge Attributes
    extract_edge_attributes <- function(network) {
      el <- network::as.edgelist(network)
      
      el <-
        el %>%
        as.data.frame() %>%
        mutate(from = as.character(factor(
          V1,
          levels = 1:attr(el, "n"),
          labels = attr(el, "vnames")
        )),
        to = as.character(factor(
          V2,
          levels = 1:attr(el, "n"),
          labels = attr(el, "vnames")
        ))) %>%
        select(-V1, -V2)
      
      edge_attr_names <- network::list.edge.attributes(network)
      edge_attr_names <- edge_attr_names[edge_attr_names != "na"]
      edge_attr_vals <-
        purrr::map(edge_attr_names, network::get.edge.value, x = network$mel)
      
      edge_attr_df <-
        bind_cols(
          el,
          edge_attr_vals,
          .name_repair = function(...)
            tidy_names(..., quiet = TRUE)
        )
      
      setNames(edge_attr_df, c("from", "to", edge_attr_names))
    }
    
    net_attrs <-
      purrr::map_dfr(x, extract_network_attributes, .id = "ego_id")
    node_attrs <-
      purrr::map_dfr(x, extract_node_attributes, .id = "ego_id")
    edge_attrs <-
      purrr::map_dfr(x, extract_edge_attributes, .id = "ego_id")
    
    extract_egos_and_return(graph_attrs = net_attrs, alters = node_attrs, 
                            edges = edge_attrs, ego_name)
  }

#' This extracts egos from igraph/network data if they are named in `ego_name`
#' and returns an egor object
#' @param graph_attrs List of graph attributes
#' @param alters alters
#' @param edges edges
#' @param ego_name ego_name
extract_egos_and_return <-
  function(graph_attrs, alters, edges, ego_name = NULL) {
    
    names(graph_attrs) <- gsub("\\.", "", names(graph_attrs))
    names(alters) <- gsub("\\.", "", names(alters))
    names(edges) <- gsub("\\.", "", names(edges))
    alters <- 
      alters %>%
      rename(name = 2)
    
    if (is.null(ego_name)) {
      egos <-
        graph_attrs
    } else if (length(ego_name) == 1) {
      edges <-
        edges %>%
        filter(from != ego_name,
               to != ego_name) %>%
        mutate(across(c(from, to), as.character))
      
      egos <-
        alters %>%
        filter(name == ego_name) %>%
        bind_cols(
          select(graph_attrs, -ego_id),
          .name_repair = function(...)
            tidy_names(..., quiet = TRUE)
        )
      
      alters <-
        alters %>%
        filter(name != ego_name)
      
    } else {
      
      edges <-
        purrr::map2_dfr(split(edges, factor(edges$ego_id, unique(edges$ego_id))),
                        ego_name,
                        ~ filter(.x,
                                 from != .y,
                                 to != .y)) %>%
        mutate(across(c(from, to), as.character))
      
      split_alters <-
        split(alters, factor(alters$ego_id, unique(alters$ego_id)))
      
      egos <-
        purrr::map2_dfr(split_alters,
                        ego_name,
                        ~ filter(.x, name == .y)) %>%
        bind_cols(
          select(graph_attrs, -ego_id),
          .name_repair = function(...)
            tidy_names(..., quiet = TRUE)
        )
      
      alters <-
        purrr::map2_dfr(split_alters,
                        ego_name,
                        ~ filter(.x, name != .y))
    }
    egor(
      alters,
      egos,
      edges,
      ID.vars = list(
        ego = "ego_id",
        alter = "name",
        source = "from",
        target = "to"
      )
    )
  }
