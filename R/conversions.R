if (getRversion() >= "2.15.1")
  utils::globalVariables(c(".tmp_row_id", "ego_id", "V1", "V2"))

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
  x <- strip_ego_design(as_nested_egor(x))
  as_igraph(x,
            directed,
            include.ego,
            ego.attrs,
            ego.alter.weights,
            graph.attrs)
}

#' @rdname as_igraph
#' @export
as_igraph.nested_egor <- function(x,
                                  directed = FALSE,
                                  include.ego = FALSE,
                                  ego.attrs = NULL,
                                  ego.alter.weights = NULL,
                                  graph.attrs = ".egoID") {
  require_igraph()
  # Create igraphs
  igraphs <-
    mapply(
      FUN = function(x, y)
        igraph::graph.data.frame(
          d = x[, names(x) != ".egoID"],
          vertices = y[, names(y) != ".egoID"],
          directed = directed
        ),
      x$.aaties,
      x$.alts,
      SIMPLIFY = FALSE
    )
  
  
  # Set graph attributes
  igraphs <-
    purrr::map2(igraphs,
                split(x[graph.attrs], 1:nrow(x)),
                function(graph, attrs) {
                  for (i in 1:ncol(attrs)) {
                    graph <- igraph::set_graph_attr(graph, names(attrs)[i], attrs[[i]])
                  }
                  graph
                })
  
  # Include Ego
  if (include.ego) {
    if (is.null(ego.attrs))
      ego.attrs <- 0
    if (is.null(ego.alter.weights))
      ego.alter.weights <- 0
    add_ego_igraph <- function(graph, alts, aaties, ego.attrs.df) {
      # Add ego vertex
      graph <- igraph::add_vertices(
        graph = graph,
        nv = 1,
        name  = "ego",
        attr = data.frame(lapply(ego.attrs.df, as.character),
                          stringsAsFactors = FALSE)
      )
      
      # Add ego alter edges
      alter_names <-
        names(igraph::V(graph))[-length(igraph::V(graph))]
      ego_edges <- Reduce(c, sapply(
        alter_names,
        FUN = function(x)
          c("ego", x),
        simplify = TRUE
      ))
      graph <- igraph::add_edges(graph,
                                 ego_edges,
                                 attr = alts[ego.alter.weights])
      # Return
      graph
    }
    
    igraphs <- mapply(
      FUN = add_ego_igraph,
      igraphs,
      x$.alts,
      x$.aaties,
      split(x[ego.attrs],
            rownames(x)),
      SIMPLIFY = FALSE
    )
  }
  # Return
  igraphs
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
  x <- strip_ego_design(as_nested_egor(x))
  
  # Include Ego
  if (include.ego) {
    if (is.null(ego.attrs))
      ego.attrs <- 0
    
    # Add ego vertex
    add_ego_network <- function(obj) {
      #obj$.alts[[1]]$.altID <- as.character(obj$.alts[[1]]$.altID)
      obj$.alts[[1]] <- mutate_all(obj$.alts[[1]], as.character)
      alt_rows <- mutate_all(obj[ego.attrs], as.character)
      obj$.alts[[1]] <- dplyr::bind_rows(obj$.alts[[1]],
                                         data.frame(
                                           .altID = "ego",
                                           alt_rows,
                                           stringsAsFactors = FALSE
                                         ))
      
      # Add ego alter edges
      alter_names <- as.character(obj$.alts[[1]]$.altID)
      len <- length(alter_names)
      alter_names <- alter_names[-len]
      obj$.aaties[[1]]$.srcID <-
        as.character(obj$.aaties[[1]]$.srcID)
      obj$.aaties[[1]]$.tgtID <-
        as.character(obj$.aaties[[1]]$.tgtID)
      
      if (!is.null(ego.alter.weights)) {
        ea_weights <- obj$.alts[[1]][ego.alter.weights][1:(len - 1),]
        ea_weights <-
          mutate_all(tibble(ea_weights = ea_weights), as.character)
        suppressWarnings(obj$.aaties[[1]] <- dplyr::mutate_at(
          obj$.aaties[[1]],
          dplyr::vars(dplyr::one_of(ego.alter.weights)),
          as.character
        ))
      }
      ea_rows <- data.frame(
        .srcID = "ego",
        .tgtID = alter_names,
        stringsAsFactors = FALSE
      )
      if (!is.null(ego.alter.weights))
        ea_rows <- dplyr::bind_cols(ea_rows, ea_weights)
      
      
      obj$.aaties[[1]] <-
        dplyr::bind_rows(obj$.aaties[[1]], ea_rows)
      obj
    }
    
    x <-
      do.call(rbind, lapply(split(x, x$.egoID), FUN = add_ego_network))
  }
  
  
  network.data.frame <- function(aaties, alts) {
    alts <- alts[, names(alts) != ".egoID"]
    aaties <- aaties[, names(aaties) != ".egoID"]
    
    aaties <- 
      mutate(aaties, across(c(.srcID, .tgtID), as.character))
    
    if (nrow(aaties) == 0) {
      n <- network::network.initialize(0)
    } else {
      all_alt_IDs <- alts$.altID
      mt <- matrix(0,
                   (la <- length(all_alt_IDs)),
                   la,
                   dimnames = list(all_alt_IDs, all_alt_IDs))
      for (i in all_alt_IDs) {
        mt[colnames(mt) == i, colnames(mt) %in% aaties[aaties$.srcID == i ,]$.tgtID] <-
          1
        if (!directed)
          mt[colnames(mt) %in% aaties[aaties$.srcID == i ,]$.tgtID, colnames(mt) == i] <-
            1
      }
      n <- network::network(mt, directed = FALSE)
    }
    for (i in 1:NCOL(alts))
      n <-
      network::set.vertex.attribute(n, names(alts)[i], as.character(alts[[i]]))
    if (NCOL(aaties) > 2)
      
      el <- network::as.edgelist(n)
      
      aaties_for_extraction <-
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
        left_join(aaties, by = c("from" = ".srcID", "to" = ".tgtID")) %>%
        select(-V1, -V2)
    
    for (i in 3:(NCOL(aaties_for_extraction)))
      n <-
        network::set.edge.attribute(n, names(aaties_for_extraction)[i], 
                                    as.character(aaties_for_extraction[[i]]))
      n
  }
  #network.data.frame(aaties = x$.aaties[[1]], alts = x$.alts[[1]])
  networks <- mapply(FUN = network.data.frame,
                     x$.aaties,
                     x$.alts,
                     SIMPLIFY = FALSE)
  
  # Set network attributes
  
  for (attr_name in graph.attrs) {
    attr <- unlist(x[attr_name])
    if (is.factor(attr))
      attr <- as.character(attr)
    networks <- purrr::map2(networks,
                            attr,
                            function(network, value)
                              network::set.network.attribute(network, attr_name, value))
  }
  
  
  # Return
  networks
}

#' @rdname as_igraph
#' @exportS3Method network::as.network egor
#' @method as.network egor
as.network.egor <- as_network

#' Extract ego, alter, and alter-alter tables from an `egor` object.
#'
#' @description Provided an `egor` object, these functions create a "global" `tibble` or `srvyr`'s [`tbl_svy`] object
#' containing egos, alter attributes, or alter-alter relations. The resulting tables
#' are useful for advanced analysis procedures, e.g. multi-level regressions.
#'
#' @description [as_tibble()] method for `egor` extracts the currently active component (`ego`, `alter`, or `aaties`) table, optionally joining it with the others, dropping any survey design information.
#'
#' @param x,object,.data An `egor` object.
#' @param include.ego.vars Logical, specifying if ego variables should be included in the result.
#' @param include.alter.vars Logical, specifying if alter variables should be included in the result.
#' @param ... Additional arguments, currently unused.
#' @examples
#' # Load example data
#' data(egor32)
#'
#' as_tibble(egor32) # Ego table.
#'
#' egor32 %>%
#'  activate("alter") %>%
#'  as_tibble(include.ego.vars=TRUE) # Alter table, but also with ego variables.
#'
#' @return A `tibble` for the `as_tibble` and `*_df` functions and a `tbl_svy` for `as_survey` and the `*_survey` functions.
#' @export
as_tibble.egor <- function(x,
                           ...,
                           include.ego.vars = FALSE,
                           include.alter.vars = FALSE) {
  res <- as_tibble(x[[attr(x, "active")]])
  
  if (include.ego.vars && attr(x, "active") != "ego") {
    ego <- if (has_ego_design(x))
      x$ego$variables
    else
      x$ego
    
    names(ego)[names(ego) != ".egoID"] <-
      paste0(names(ego)[names(ego) != ".egoID"] , "_ego")
    res <- full_join(res, ego, by = ".egoID")
  }
  
  if (include.alter.vars & attr(x, "active") == "aatie") {
    res <- left_join(res,
                     x$alter,
                     by = c(".egoID", ".srcID" = ".altID"))
    res <- left_join(
      res,
      x$alter,
      by = c(".egoID", ".tgtID" = ".altID"),
      suffix = c("_src", "_tgt")
    )
  }
  
  res
}

#' @rdname as_tibble.egor
#' @description [as_survey()] method for `egor` instead returns a `srvyr` [`tbl_svy`] survey, taking into account any replication due to multiple alters or alter-alter ties incident on each ego. If no design is specified for the egos, then the default design (simple random sample with replacement) is assumed as the starting point.
#' @examples
#' library(srvyr)
#' as_survey(egor32) # Ego table with survey design.
#' @importFrom srvyr as_survey
#' @export
as_survey.egor <- function(.data,
                           ...,
                           include.ego.vars = FALSE,
                           include.alter.vars = FALSE) {
  if (!has_ego_design(.data))
    .data$ego <- as_survey(.data$ego)
  # Obtain the results ignoring design.
  result <- as_tibble(.data,
                      ...,
                      include.ego.vars = include.ego.vars,
                      include.alter.vars = include.alter.vars)
  # Now, figure out to which original ego row each of the output rows corresponds.
  emap <- match(result$.egoID, .data$ego$variables$.egoID)
  # Augment the initial ego survey design
  result.design <- .data$ego[emap, ]
  result.design$variables <- result
  result.design
}

#' @rdname as_tibble.egor
#' @description `as_egos_df()`, `as_alters_df()`, `as_aaties_df()`, `as_egos_survey()`, `as_alters_survey()`, and `as_aaties_survey()` are convenience functions for the `as_tibble()` and `as_survey()` methods, activating the corresponding component of the `egor` object.
#' @examples
#'
#' # Despite alter table being active, obtain the ego table.
#' (egor32 <- activate(egor32, "alter"))
#' as_egos_df(egor32)
#'
#' @export
as_egos_df <- function(object) {
  object <- activate(object, "ego")
  as_tibble(object)
}

#' @rdname as_tibble.egor
#' @examples
#' # Create global alter table
#' as_alters_df(egor32)
#'
#' @export
as_alters_df <- function(object, include.ego.vars = FALSE) {
  object <- activate(object, "alter")
  as_tibble(object, include.ego.vars = include.ego.vars)
}

#' @rdname as_tibble.egor
#' @examples
#' # Create global alter-alter relations table
#' as_aaties_df(egor32)
#'
#' # ... adding alter variables
#' as_aaties_df(egor32, include.alter.vars = TRUE)
#' @export
#' @importFrom dplyr left_join
#' @importFrom purrr map_lgl
as_aaties_df <- function(object,
                         include.ego.vars = FALSE,
                         include.alter.vars = FALSE) {
  object <- activate(object, "aatie")
  as_tibble(object,
            include.ego.vars = include.ego.vars,
            include.alter.vars = include.alter.vars)
}

#' @rdname as_tibble.egor
#' @examples
#' as_egos_survey(egor32)
#' @export
as_egos_survey <- function(object, include.ego.vars = FALSE) {
  object <- activate(object, "ego")
  as_survey(object, include.ego.vars = include.ego.vars)
}

#' @rdname as_tibble.egor
#' @examples
#' as_alters_survey(egor32) # Notice the resulting cluster design.
#' @export
as_alters_survey <- function(object, include.ego.vars = FALSE) {
  object <- activate(object, "alter")
  as_survey(object, include.ego.vars = include.ego.vars)
}

#' @rdname as_tibble.egor
#' @export
as_aaties_survey <- function(object,
                             include.ego.vars = FALSE,
                             include.alter.vars = FALSE) {
  object <- activate(object, "aatie")
  as_survey(object,
            include.ego.vars = include.ego.vars,
            include.alter.vars = include.alter.vars)
}
