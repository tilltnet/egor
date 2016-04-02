#' Cluster ego-centered networks by a factor and visualise the results
#'
#' The idea of clustered graphs in ego-centered network analysis is developed by
#' Lerner et al. (2008). It helps to discover and visualise structural and 
#' compostional properties of ego-centered networks, based on a pre-defined
#' factor variable on the alter level, i.e. the country of origin.
#' @param alteri.list \code{List} of \code{data frames} containing the alteri 
#' data.
#' @param edges.list \code{List} of \code{data frames} containing the edge 
#' lists.
#' @param clust.groups A \code{factor} variable the build the clusters from.
#' @param graphs \code{List} of \code{graph} objects, representing the clustered
#' graphs.
#' @param vertex.min.size \code{Numeric} indicating minimum size of plotted 
#' vertices.
#' @param vertex.max.size \code{Numeric} indicating maximum size of plotted 
#' vertices.
#' @param center \code{Numeric} indicating the vertex to be plotted in center.
#' @param labels \code{Boolean}. Plots with turned off labels will be preceeded 
#' by a 'legend' plot giving the labels of the vertices.
#' @param to.pdf \code{Boolean}.
#' @references Brandes, U., Lerner, J., Lubbers, M. J., McCarty, C., & Molina, 
#' J. L. (2008). Visual Statistics for Collections of Clustered Graphs. 2008 
#' IEEE Pacific Visualization Symposium, 47-54.
#' @return \code{clustered.graphs} returns a list of graph objects representing 
#' the clustered ego-centered network data; \code{vis.clustered.graphs} plots
#' the clustered graphs (into a pdf file, if indicated)
#' @keywords ego-centric network analysis
#' @export
clustered.graphs <- function(alteri.list, edges.list, clust.groups) {
  GetGroupSizes <- function(x) {
    y <- aggregate(x$alterID, by = x[clust.groups], FUN = NROW)
    names(y) <- c("groups", "size")
    y
  }
  
  alteri.grped.list <- lapply(alteri.list, FUN = GetGroupSizes)
  graphs <- to.network(e.lists = edges.list, alteri.list = alteri.list)
  
  
  
  
  ## Extracting edges within and between groups ------------------------------
  
  calculateGrpDensities <- function(g, alteri.group.n, clust.groups) {
    SelectGroupEdges <- function(g, clust.groups, group1, group2 = group1) {
      V.group1 <- igraph::V(g)[igraph::get.vertex.attribute(g, clust.groups) == group1]
      V.group2 <- igraph::V(g)[igraph::get.vertex.attribute(g, clust.groups) == group2]
      igraph::E(g)[V.group1 %--% V.group2]
    }
    
    x_names <- names(table(igraph::get.vertex.attribute(g, clust.groups)))
    x_dim <- length(x_names)
    
    for.loop.matrix <- matrix(1, ncol = x_dim, nrow = x_dim)
    colnames(for.loop.matrix) <- x_names
    rownames(for.loop.matrix) <- x_names
    
    groups.list <- list()
    grps.df <- data.frame()
    for (i in 1:x_dim) {
      i.name <- colnames(for.loop.matrix)[i]
      for (j in (1-1+i):(x_dim)) {
        j.name <- rownames(for.loop.matrix)[j]
        ij.name <- paste (i.name, j.name)
        groups.list[[ij.name]] <- SelectGroupEdges(g, clust.groups, i.name, j.name)
        grp.size <- length(groups.list[[ij.name]])
        groups.size.i <- alteri.group.n$size[alteri.group.n$groups == i.name]
        groups.size.j <- alteri.group.n$size[alteri.group.n$groups == j.name]
        if(j.name != i.name) {
          grp.possible.dyads <- dyads.possible.between.groups(groups.size.i, groups.size.j)
        } else {
          grp.possible.dyads <- dyad.poss(groups.size.i)
        }
        grp.density <- grp.size / grp.possible.dyads
        #grp.density.fake <- sample(0:100/100, 10)
        
        grps.df <- rbind(grps.df, data.frame(i.name, j.name, grp.size, grp.possible.dyads, grp.density))
        
        
      }
    }
    list(grp.densities = grps.df, edges.lists = groups.list)
  }
  
  grp.densities <- mapply(FUN = calculateGrpDensities, graphs, alteri.grped.list, clust.groups, SIMPLIFY = F)
  
  ## Create 'clustered graphs' igraph object  --------------------------------
  
  
  clustered.graphs <- lapply(grp.densities, 
                             FUN = function(x) igraph::graph.data.frame(
                               x$grp.densities[x$grp.densities$i.name != x$grp.densities$j.name, ],
                               vertices = x$grp.densities[x$grp.densities$i.name == x$grp.densities$j.name, ],
                               directed = F))
  clustered.graphs
}


#' @describeIn clustered.graphs Visualises clustered.graphs using the formerly
#' created clustered graph objects.
#' @export
vis.clustered.graphs <- function(graphs, vertex.min.size, vertex.max.size,
                               center = 1, labels = F, to.pdf = T) {

    plotLegendGraph <- function(grps.graph, center) {
      igraph::plot.igraph(grps.graph, 
                vertex.color = "grey", 
                vertex.frame.color = NA, 
                vertex.size = 25,
                edge.width = 5,
                vertex.label.color = "black", 
                vertex.label.cex = 4,
                vertex.label.family = "sans",
                vertex.label.dist = 4,
                vertex.label.degree = ifelse(igraph::layout.star(grps.graph, center = center)[,1] >= 1, 0, pi),
                layout = igraph::layout.star(grps.graph, center = center))
  }
  
  plotGraph <- function(graph, center) {
    if(labels) {
      vertex.label <- paste(" ", igraph::V(graph)$grp.size,
                            round(igraph::V(graph)$grp.density, digits = 2), sep = "\n")
      vertex.label.b <- paste(igraph::V(graph)$name, " ",  " ", sep = "\n")
      edge.label <- round(igraph::E(graph)$grp.density, digits = 2)
      grey.shades <- gray(9:2/10)[igraph::V(graph)$grp.density*10]
      grey.shades <-  strtoi(substr(gsub("#", replacement = "0x", grey.shades), start = 1, stop = 4))
      label.shades <- ifelse(grey.shades < 120, "white", "black")
    } else {
      vertex.label <- NA
      vertex.label.b <- NA
      edge.label <- NA
      label.shades <- NA
    }
    
    vertex.size <- igraph::V(graph)$grp.size
    vertex.size <- ifelse(vertex.size < vertex.min.size, vertex.min.size, vertex.size)
    vertex.size[vertex.size > vertex.max.size] <- vertex.max.size
    
    igraph::plot.igraph(graph, 
                vertex.color = gray(9:2/10)[igraph::V(graph)$grp.density*10], 
                vertex.frame.color = NA, 
                vertex.size = vertex.size,
                vertex.label.color = label.shades, 
                vertex.label.cex = 0.8,
                vertex.label = vertex.label,
                vertex.label.family = "sans",
                vertex.label.font = 1,
                edge.width = igraph::E(graph)$grp.density*30,
                edge.arrow.size = 0, 
                edge.label = edge.label,
                edge.label.color = edge.label.color,
                edge.label.cex = edge.label.cex,
                edge.label.family = "sans",
                layout = igraph::layout.star(graph, center = center)) # Test Layout with 3 groups/ 2 groups, more groups
    
    igraph::plot.igraph(graph, add = T,
                vertex.color = NA, 
                vertex.frame.color = NA, 
                vertex.size = vertex.size,
                vertex.label.color = label.shades, 
                vertex.label.cex = 0.8,
                vertex.label = vertex.label.b,
                vertex.label.family = "serif",
                vertex.label.font = 2,
                edge.width = 0,
                edge.color = NA,
                edge.arrow.size = 0, 
                layout = igraph::layout.star(graph, center = center))
    #print(label.shades)
    #print(grey.shades)
  }
  
  example.graph <- graphs[[1]]
  center.vertex.max <- length(igraph::V(example.graph))
  
  if(to.pdf) {
    rand.chars <- paste(sample(c(0:9, letters, LETTERS),
                               8, replace=TRUE), collapse="")
    filename <- paste("clustered_graphs_" , rand.chars, ".pdf")
    pdf(file=filename, width = 46.81, height = 33.11)
    
    page.xy <- din.page.dist(length(graphs) + 1)
    par(mfrow=c(page.xy[1], page.xy[2]))
  }
  
  if(!labels) {
    plotLegendGraph(example.graph, 1)
  }
  
  edge.label.color <- "black"
  edge.label.cex <- 0.7
  for(graph in graphs) {
    plotGraph(graph, center)
  }
  
  if(to.pdf) {
    dev.off()
  }
}