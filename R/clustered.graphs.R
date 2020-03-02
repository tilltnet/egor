#' Cluster ego-centered networks by a grouping factor
#'
#' The idea of clustered graphs is to reduce the complexity of an ego-centered network
#' graph by visualizing alters in clusters defined by a categorical variable (Lerner et al. 2008).
#' \code{clustered_graphs()} calculates group sizes, inter and intra group tie
#' densities and returns these informations in a \code{list} of \code{igraph} objects.
#' @template object
#' @template aaties
#' @template egoID
#' @param clust.groups A \code{character} naming the \code{factor} variable defining the groups.
#' @template meth_dots
#' @references Brandes, U., Lerner, J., Lubbers, M. J., McCarty, C., & Molina,
#' J. L. (2008). Visual Statistics for Collections of Clustered Graphs. 2008
#' IEEE Pacific Visualization Symposium, 47-54.
#' @return \code{clustered_graphs} returns a list of graph objects representing
#' the clustered ego-centered network data;
#' @keywords ego-centered network analysis
#' @seealso \code{\link{vis_clustered_graphs}} for visualizing clustered graphs
#' @example /inst/examples/ex_cg.R
#' @importFrom igraph %--%
#' @export
clustered_graphs <-
  function(object, ..., clust.groups)
    UseMethod("clustered_graphs", object)

#' @rdname clustered_graphs
#' @export
clustered_graphs.list <-
  function(object, aaties, clust.groups, ...) {
    GetGroupSizes <- function(x) {
      if (all(is.na(x[[clust.groups]])))
        return(data.frame(groups = NA, size = NA))
      y <- data.frame(table(x[clust.groups]))
      names(y) <- c("groups", "size")
      y
    }
    alters.grped.list <- lapply(object, FUN = GetGroupSizes)
    
    # Exclude NAs in clust.groups
    alters <-
      lapply(
        object,
        FUN = function(y)
          y[!is.na(y[clust.groups][[1]]),]
      )
    
    # Filter out alters in edge list, that are not in alter data
    aaties <- mapply(
      FUN = function(x, y) {
        x <- x[x[[1]] %in% y[[1]],]
        x <- x[x[[2]] %in% y[[1]],]
        x
      },
      aaties,
      alters,
      SIMPLIFY = FALSE
    )
    
    graphs <- mapply(
      FUN = function(x, y)
        igraph::graph.data.frame(
          d = x,
          vertices = y,
          directed = FALSE
        ),
      aaties,
      alters,
      SIMPLIFY = FALSE
    )
    
    # Extracting edges within and between groups ------------------------------
    
    calculateGrpDensities <-
      function(g, alters.group.n, clust.groups) {
        SelectGroupEdges <-
          function(g, clust.groups, group1, group2 = group1) {
            V.group1 <-
              igraph::V(g)[igraph::get.vertex.attribute(g, clust.groups) == group1]
            V.group2 <-
              igraph::V(g)[igraph::get.vertex.attribute(g, clust.groups) == group2]
            igraph::E(g)[V.group1 %--% V.group2]
          }
        
        
        # Check if all groups are zero sized, if so: return empty entries for grp.df and asdad
        if (length(igraph::V(g)) < 1) {
          groups.list <- list()
          grps.df <-
            data.frame(
              i.name = character(0),
              j.name = character(0),
              grp.size = numeric(0),
              grp.possible.dyads = numeric(0),
              grp.density = numeric(0)
            )
          
          
        } else {
          x_names <-
            names(table(igraph::get.vertex.attribute(g, clust.groups)))
          x_dim <- length(x_names)
          
          for.loop.matrix <- matrix(1, ncol = x_dim, nrow = x_dim)
          colnames(for.loop.matrix) <- x_names
          rownames(for.loop.matrix) <- x_names
          
          groups.list <- list()
          grps.df <- data.frame()
          for (i in 1:x_dim) {
            i.name <- colnames(for.loop.matrix)[i]
            for (j in (1 - 1 + i):(x_dim)) {
              j.name <- rownames(for.loop.matrix)[j]
              ij.name <- paste(i.name, j.name)
              
              groups.list[[ij.name]] <-
                SelectGroupEdges(g, clust.groups, i.name, j.name)
              real.dyads <- length(groups.list[[ij.name]])
              groups.size.i <-
                alters.group.n$size[alters.group.n$groups == i.name]
              groups.size.j <-
                alters.group.n$size[alters.group.n$groups == j.name]
              
              
              grp.size <-
                ifelse(i.name == j.name,
                       groups.size.i,
                       groups.size.i + groups.size.j)
              
              
              if (j.name != i.name) {
                grp.possible.dyads <-
                  dyads_possible_between_groups(groups.size.i, groups.size.j)
              } else {
                grp.possible.dyads <- dyad.poss(groups.size.i)
              }
              grp.density <- real.dyads / grp.possible.dyads
              grps.df <-
                rbind(
                  grps.df,
                  data.frame(
                    i.name,
                    j.name,
                    grp.size,
                    grp.possible.dyads,
                    grp.density
                  )
                )
              
              
            }
          }
          # Check for empty categories and add dummy vertex.
          empty_cats <-
            alters.group.n$groups[!alters.group.n$groups %in% grps.df$i.name]
          if (length(empty_cats) > 0) {
            for (i in 1:length(empty_cats)) {
              empty_dummy <- data.frame(empty_cats[i], empty_cats[i], 0, NaN, NA)
              names(empty_dummy) <- names(grps.df)
              grps.df <- rbind(grps.df, empty_dummy)
            }
          }
        }
        
        list(grp.densities = grps.df, aatiess = groups.list)
      }
    
    grp.densities <-
      mapply(FUN = calculateGrpDensities,
             graphs,
             alters.grped.list,
             clust.groups,
             SIMPLIFY = FALSE)
    
    # Create 'clustered graphs' igraph object  --------------------------------
    
    
    clustered_graphs <- lapply(
      grp.densities,
      FUN = function(x)
        igraph::graph.data.frame(
          x$grp.densities[x$grp.densities$i.name != x$grp.densities$j.name,],
          vertices = x$grp.densities[x$grp.densities$i.name == x$grp.densities$j.name,],
          directed = FALSE
        )
    )
    clustered_graphs
  }

#' @rdname clustered_graphs
#' @export
clustered_graphs.egor <- function(object, clust.groups, ...) {
  object <- as_nested_egor(object)
  clustered_graphs(
    object = object$.alts,
    aaties = object$.aaties,
    clust.groups = clust.groups
  )
}

#' @rdname clustered_graphs
#' @export
clustered_graphs.data.frame <-
  function(object, aaties, clust.groups, egoID = ".egoID", ...) {
    alters <- split(object, object[[egoID]])
    aaties <- split(aaties, aaties[[egoID]])
    alters <- lapply(
      alters,
      FUN = function(x)
        select(x, -.egoID)
    )
    aaties <- lapply(
      aaties,
      FUN = function(x)
        x[2:NCOL(x)]
    )
    
    clustered_graphs(object = alters, 
                     aaties = aaties, 
                     clust.groups = clust.groups)
  }

#' Visualize clustered graphs
#'
#' \code{vis_clustered_graphs} visualizes clustered_graphs using a list of
#' clustered graphs created with \code{\link{clustered_graphs}}.
#' @param graphs \code{List} of \code{graph} objects, representing the clustered
#' graphs.
#' @param node.size.multiplier \code{Numeric} used to multiply the node diameter
#' of visualized nodes.
#' @param node.min.size \code{Numeric} indicating minimum size of plotted
#' nodes
#' @param node.max.size \code{Numeric} indicating maximum size of plotted
#' nodes
#' @param edge.width.multiplier \code{Numeric} used to multiply the edge width.
#' @param center \code{Numeric} indicating the vertex to be plotted in center.
#' @param label.size \code{Numeric}.
#' @param labels \code{Boolean}. Plots with turned off labels will be preceeded
#' by a 'legend' plot giving the labels of the vertices.
#' @param legend.node.size \code{Numeric} used as node diameter of legend graph.
#' @param pdf.name \code{Character} giving the name/path of the pdf file to create.
#' @param ... Arguments to pass to `plot.igraph`.
#' @return \code{vis_clustered_graphs} plots
#' a \code{list} of \code{igraph} objects created by the \code{clustered_graphs}
#' function.
#' @references Brandes, U., Lerner, J., Lubbers, M. J., McCarty, C., & Molina,
#' J. L. (2008). Visual Statistics for Collections of Clustered Graphs. 2008
#' IEEE Pacific Visualization Symposium, 47-54.
#' @return \code{clustered_graphs} returns a list of graph objects representing
#' the clustered ego-centered network data;
#' @keywords ego-centered network analysis
#' @seealso \code{\link{clustered_graphs}} for creating clustered graphs objects
#' @example /inst/examples/ex_cg.R
#' @export
vis_clustered_graphs <- function(graphs,
                                 node.size.multiplier = 1,
                                 node.min.size = 0,
                                 node.max.size = 200,
                                 edge.width.multiplier = 30,
                                 center = 1,
                                 label.size = 0.8,
                                 labels = FALSE,
                                 legend.node.size = 45,
                                 pdf.name = NULL,
                                 ...) {
  plotLegendGraph <- function(grps.graph, center) {
    # set all edges to 1
    vertex_names <- names(igraph::V(grps.graph))
    vertex_df <- data.frame(x1 = vertex_names)
    vertex_length <- length(vertex_names)
    edges_mat <-
      matrix(
        1,
        nrow = vertex_length,
        ncol = vertex_length,
        dimnames = list(vertex_names, vertex_names)
      )
    diag(edges_mat) <- 0
    edges_mat[upper.tri(edges_mat)] <- 0
    edges_graph <- igraph::graph_from_adjacency_matrix(edges_mat)
    edge_list <-
      igraph::ends(edges_graph, igraph::E(edges_graph), names = TRUE)
    grps.graph <-
      igraph::graph.data.frame(d = edge_list,
                               vertices = vertex_df,
                               directed = FALSE)
    
    igraph::plot.igraph(
      grps.graph,
      vertex.color = "grey",
      vertex.frame.color = NA,
      vertex.size = legend.node.size,
      edge.width = 1,
      vertex.label.color = "black",
      vertex.label.cex = label.size,
      vertex.label.family = "sans",
      
      layout = layout_,
      ...
    )
  }
  
  plotGraph <- function(graph, center) {
    if (labels) {
      vertex.label <- paste(" ",
                            igraph::V(graph)$grp.size,
                            round(igraph::V(graph)$grp.density, digits = 2),
                            sep = "\n")
      vertex.label.b <-
        paste(igraph::V(graph)$name, " ",  " ", sep = "\n")
      edge.label <-
        ifelse(
          igraph::E(graph)$grp.density == 0,
          "" ,
          round(igraph::E(graph)$grp.density, digits = 2)
        )
      
      #' @importFrom grDevices gray
      grey.shades <-
        gray(seq(1, 0, -0.008))[igraph::V(graph)$grp.density * 100 + 1]
      grey.shades <-
        strtoi(substr(
          gsub("#", replacement = "0x", grey.shades),
          start = 1,
          stop = 4
        ))
      label.shades <- ifelse(grey.shades < 120, "#cccccc", "black")
      label.shades.b <- ifelse(grey.shades > 120, "white", "black")
      if (length(label.shades) == 0)
        label.shades <- "black"
    } else {
      vertex.label <- NA
      vertex.label.b <- NA
      edge.label <- NA
      label.shades <- NA
    }
    
    vertex.size <-
      igraph::V(graph)$grp.size * node.size.multiplier + node.min.size
    vertex.size[vertex.size > node.max.size] <- node.max.size
    
    
    
    igraph::plot.igraph(
      graph,
      vertex.color = gray(seq(1, 0, -0.008))[igraph::V(graph)$grp.density *
                                               100 + 1],
      vertex.frame.color = ifelse(
        igraph::V(graph)$grp.density == 0 |
          is.na(igraph::V(graph)$grp.density),
        "black",
        NA
      ),
      vertex.size = vertex.size,
      vertex.label.color = label.shades,
      vertex.label.cex = label.size,
      vertex.label = vertex.label,
      vertex.label.family = "sans",
      vertex.label.font = 1,
      edge.width = igraph::E(graph)$grp.density * edge.width.multiplier,
      edge.arrow.size = 0,
      edge.label = edge.label,
      edge.label.color = "black",
      edge.label.cex = edge.label.cex,
      edge.label.family = "sans",
      edge.color = ifelse(igraph::E(graph)$grp.density == 0, NA, "grey"),
      layout = layout_,
      ...
    )
    
    
    igraph::plot.igraph(
      graph,
      add = TRUE,
      vertex.color = NA,
      vertex.frame.color = NA,
      vertex.size = vertex.size,
      vertex.label.color = label.shades,
      vertex.label.cex = label.size,
      vertex.label = vertex.label.b,
      vertex.label.family = "serif",
      vertex.label.font = 2,
      edge.width = 0,
      edge.color = NA,
      edge.arrow.size = 0,
      layout = layout_,
      ...
    )
    
  }
  
  example.graph <- graphs[[1]]
  center.vertex.max <- length(igraph::V(example.graph))
  
  if (!missing(pdf.name)) {
    #' @importFrom grDevices pdf
    pdf(file = pdf.name,
        width = 46.81,
        height = 33.11)
    
    page.xy <- din_page_dist(length(graphs) + 1)
    #' @importFrom graphics par
    par(mfrow = c(page.xy[1], page.xy[2]))
  }
  
  if (!labels) {
    if (length(igraph::V(example.graph)) < 4) {
      layout_ <- igraph::layout.circle
    } else {
      layout_ <- igraph::layout_as_star(example.graph, center = center)
    }
    plotLegendGraph(example.graph, 1)
  }
  
  
  edge.label.cex <- label.size
  edge.label.color <- "black"
  for (graph in graphs) {
    if (length(igraph::V(graph)) < 1) {
      #' @importFrom graphics plot.new
      plot.new()
    } else {
      if (length(igraph::V(graph)) < 4) {
        layout_ <- igraph::layout.circle
      } else {
        layout_ <- igraph::layout_as_star(graph, center = center)
      }
      plotGraph(graph, center)
    }
    
  }
  
  if (!missing(pdf.name)) {
    #' @importFrom grDevices dev.off
    dev.off()
  }
}
