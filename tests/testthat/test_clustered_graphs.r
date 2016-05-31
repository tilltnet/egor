library(egonetR)

mpf <- generate.sample.ego.data(20, 50)

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

# Test if empty categories break.
a <- alteri.list[[1]]
NROW(a)
a <- a[a$random.groups != "GER",]
NROW(a)

e <- edges.list[[1]]
NROW(e)
e <- e[e$Source %in% a$alterID & e$Target %in% a$alterID, ]
NROW(e)

alteri.list[[1]] <- a
edges.list[[1]] <- e

graphs_ex <- clustered.graphs(alteri.list, edges.list, "random.groups") 
vis.clustered.graphs(graphs, vertex.min.size = 45, vertex.max.size = 200,
                     labels = T, to.pdf = F)


# LoMiHi - Low Middle High (+varieties)  
lomihi <- generate.sample.ego.data(net.count = 4, max.alteri = 12, netsize = 12)

a.lomihi <- split(x = lomihi$alteri, f = lomihi$alteri$egoID)
e.lomihi <- lomihi$edges
