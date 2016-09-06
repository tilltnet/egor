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

graphs_ex <- clustered.graphs(alteri.list[1], edges.list[1], "random.groups") 
vis.clustered.graphs(graphs_ex, vertex.min.size = 45, vertex.max.size = 200,
                     labels = F, to.pdf = F)


# LoMiHi - Low Middle High (+varieties)  
lomihi <- generate.sample.ego.data(net.count = 6, max.alteri = 120, netsize = 120)
lomihi$alteri$alter.age <- factor(lomihi$alteri$alter.age)
a.lomihi <- split(x = lomihi$alteri, f = lomihi$alteri$egoID)
a.lomihi <- lapply(a.lomihi, function(x) x[2:4])
e.lomihi <- lomihi$edges

# No edges
e.lomihi[[1]] <- e.lomihi[[1]][e.lomihi[[1]]$weight == 0, ]
a.lomihi[[1]] <- a.lomihi[[1]][a.lomihi[[1]]$alterID < 12, ]

# No edges - no alteri in some groups
e.lomihi[[2]] <- e.lomihi[[2]][e.lomihi[[2]]$weight == 4, ]
a.lomihi[[2]] <- a.lomihi[[2]][a.lomihi[[2]]$alter.age == levels(a.lomihi[[2]]$alter.age)[1], ]

# No edges - no alteri at all
e.lomihi[[3]] <- e.lomihi[[3]][e.lomihi[[3]]$weight == 4, ]
a.lomihi[[3]] <- a.lomihi[[3]][a.lomihi[[3]]$alter.age == "x", ] # This breaks clustered.graphs()

# NAs in grouping variable
ind <- sample(1:120, 20)
a.lomihi[[4]]$alter.age[ind] <- NA
e.lomihi[[4]] <- e.lomihi[[4]][e.lomihi[[4]]$weight == 0, ]

graphs <- clustered.graphs(a.lomihi, e.lomihi, "alter.age") 

vis.clustered.graphs(graphs, vertex.min.size = 45, vertex.max.size = 200,
                     labels = T, to.pdf = F)

vis.clustered.graphs(graphs, vertex.min.size = 45, vertex.max.size = 200,
                     labels = F, to.pdf = T)

#library(igraph)
E(graphs[[4]])$grp.density



# Choose layouts for groups of two and three