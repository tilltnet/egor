library(egonetR)

mpf <- generate.sample.ego.data(20, 100)

alteri.list <- split(x = mpf$alteri, f = mpf$alteri$egoID)
edges.list <- mpf$edges


# Create nationality/ ethnicity variable
CreateRamdomGroups <- function(alteri) {
  group.names <- c("ITA", "GER", "USA", "ESP")
  random.groups <- sample(group.names, NROW(alteri), replace = TRUE)
  alteri <- data.frame(alteri, random.groups)
  alteri
}

alteri.list <- lapply(X = alteri.list, FUN = CreateRamdomGroups)

alteri.list <- lapply(X = alteri.list, FUN = function (x) x[2:5])

# Delete random edges, but not too random
deleteRandomEdges <- function(edge.list) {
  max <- nrow(edge.list)
  min <- max/2
  edge.list <- edge.list[sample(max, sample(min:max, 1)), ]
  edge.list[edge.list$weight != 3, ]
}

edges.list <- lapply(edges.list, FUN = deleteRandomEdges)


graphs <- clustered.graphs(alteri.list, edges.list, "random.groups") 

vis.clustered.graphs(graphs, vertex.min.size = 45, vertex.max.size = 200,
                   labels = T, to.pdf = F)

vis.clustered.graphs(graphs, vertex.min.size = 45, vertex.max.size = 200,
                   labels = F, to.pdf = T)
