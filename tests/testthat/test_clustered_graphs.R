cat(" \nTestfile  test_clustered_graphs.r \n")

library(egor)

# Create test data

mpf <- generate.sample.ego.data(20, 50)

alters.list <- mpf$.alts
edges.list <- mpf$.aaties


# Create nationality/ ethnicity variable
CreateRamdomGroups <- function(alters) {
  group.names <- c("ITA", "GER", "USA", "ESP")
  random.groups <- sample(group.names, NROW(alters), replace = TRUE)
  alters <- data.frame(alters, random.groups)
  alters
}

alters.list <- lapply(X = alters.list, FUN = CreateRamdomGroups)
mpf$.alts <- alters.list

# test egor -------------------------------------------------------------------
clustered_graphs(mpf, "random.groups") 


# test dataframe ----------------------------------------------------------
of2 <- as_tibble(mpf)

alters <- tidyr::unnest(dplyr::select(of2, egoID, .alts))
aaties <- tidyr::unnest(dplyr::select(of2, egoID, .aaties))
graphs <- clustered_graphs(alters, aaties, "random.groups") 


# test list ---------------------------------------------------------------

graphs <- clustered_graphs(alters.list, edges.list, "random.groups") 


# vis_clustered_graphs(graphs, node.min.size = 45, node.max.size = 200,
#                    labels = T, to.pdf = F)
# 
# vis_clustered_graphs(graphs, node.min.size = 45, node.max.size = 200,
#                    labels = F, to.pdf = T)

# Test if empty categories break.
a <- alters.list[[1]]
NROW(a)
a <- a[a$random.groups != "GER",]
NROW(a)

e <- edges.list[[1]]
NROW(e)
e <- e[e$.srcID %in% a$.altID & e$.tgtID %in% a$.altID, ]
NROW(e)

alters.list[[1]] <- a
edges.list[[1]] <- e



graphs_ex <- clustered_graphs(alters.list[1], edges.list[1], "random.groups") 
#vis_clustered_graphs(graphs_ex, node.min.size = 45, node.max.size = 200,
#                     labels = F, to.pdf = F)


# # Testing extreme data situations ---------------------------------------------
# 
# ## Create and extract data
# lomihi <- generate.sample.ego.data(net.count = 10, max.alters = 120, netsize = 120)
# lomihi$.alts$alter.age <- factor(lomihi$.alts$alter.age)
# a.lomihi <- split(x = lomihi$.alts, f = lomihi$.alts$egoID)
# a.lomihi <- lapply(a.lomihi, function(x) x[2:4])
# e.lomihi <- lomihi$edges
# 
# # No edges
# e.lomihi[[1]] <- e.lomihi[[1]][e.lomihi[[1]]$weight == 0, ]
# a.lomihi[[1]] <- a.lomihi[[1]][a.lomihi[[1]]$.altID < 12, ]
# 
# # No edges - no alters in some groups
# e.lomihi[[2]] <- e.lomihi[[2]][e.lomihi[[2]]$weight == 4, ]
# a.lomihi[[2]] <- a.lomihi[[2]][a.lomihi[[2]]$alter.age == levels(a.lomihi[[2]]$alter.age)[1], ]
# 
# # No edges - no alters at all
# e.lomihi[[3]] <- e.lomihi[[3]][e.lomihi[[3]]$weight == 4, ]
# a.lomihi[[3]] <- a.lomihi[[3]][a.lomihi[[3]]$alter.age == "x", ] # This breaks clustered_graphs()
# 
# # NAs in grouping variable
# ind <- sample(1:120, 20)
# a.lomihi[[4]]$alter.age[ind] <- NA
# e.lomihi[[4]] <- e.lomihi[[4]][e.lomihi[[4]]$weight == 0, ]
# 
# # 0.5 density
# tmp <- NROW(e.lomihi[[5]])
# ind <- sample(1:tmp, tmp/2)
# e.lomihi[[5]]<- e.lomihi[[5]][ind, ]
# 
# 
# # mixed density
# table(a.lomihi[[6]]$alter.age)
# age_levels <- levels(a.lomihi[[6]]$alter.age)
# a_1 <- which(a.lomihi[[6]]$alter.age == age_levels[1])
# e.lomihi[[6]] <- e.lomihi[[6]][e.lomihi[[6]]$.srcID %in% a_1 & e.lomihi[[6]]$.tgtID %in% a_1, ]
# a_1 <- which(a.lomihi[[7]]$alter.age == age_levels[1])
# e.lomihi[[7]] <- e.lomihi[[7]][e.lomihi[[7]]$.srcID %in% a_1 | e.lomihi[[7]]$.tgtID %in% a_1, ]
# 
# a_2 <- which(a.lomihi[[8]]$alter.age == age_levels[2])
# e.lomihi[[8]] <- e.lomihi[[8]][e.lomihi[[8]]$.srcID %in% a_2 & e.lomihi[[8]]$.tgtID %in% a_2, ]
# a_2 <- which(a.lomihi[[9]]$alter.age == age_levels[2])
# e.lomihi[[9]] <- e.lomihi[[9]][e.lomihi[[9]]$.srcID %in% a_2 | e.lomihi[[9]]$.tgtID %in% a_2, ]
# 
# tmp <- NROW(e.lomihi[[10]])
# ind <- sample(1:tmp, tmp/2)
# e.lomihi_copy <- e.lomihi[[10]][ind, ]
# age_levels <- levels(a.lomihi[[10]]$alter.age)
# a_1 <- which(a.lomihi[[10]]$alter.age == age_levels[1])
# e.lomihi[[10]] <- e.lomihi_copy[e.lomihi_copy$.srcID %in% a_1 & e.lomihi_copy$.tgtID %in% a_1, ]
# e.lomihi[[11]] <- e.lomihi_copy[e.lomihi_copy$.srcID %in% a_1 | e.lomihi_copy$.tgtID %in% a_1, ]
# e.lomihi[[12]] <- e.lomihi_copy[e.lomihi_copy$.srcID %in% a_2 & e.lomihi_copy$.tgtID %in% a_2, ]
# e.lomihi[[13]] <- e.lomihi_copy[e.lomihi_copy$.srcID %in% a_2 | e.lomihi_copy$.tgtID %in% a_2, ]
# a.lomihi[11:13] <- a.lomihi[10]
# 
# a_2 <- which(a.lomihi[[10]]$alter.age == age_levels[2])
# e.lomihi[[14]] <- e.lomihi_copy[e.lomihi_copy$.srcID %in% a_1 | e.lomihi_copy$.tgtID %in% a_1 | e.lomihi_copy$.tgtID %in% a_2 | e.lomihi_copy$.tgtID %in% a_2, ]
# a.lomihi[14] <- a.lomihi[10]
# 
# graphs <- clustered_graphs(a.lomihi, e.lomihi, "alter.age") 
# #E(graphs[[6]])$grp.density
# vis_clustered_graphs(graphs)
# vis_clustered_graphs(graphs, node.size.multiplier = 2, node.max.size = 200, edge.width.multiplier = 40,
#                      label.size = 0.8)
# 
# 
# vis_clustered_graphs(graphs, node.size.multiplier = 1, node.min.size = 45, node.max.size = 200, edge.width.multiplier = 40,
#                      labels = T, label.size = 1, to.pdf = F)
# 
# vis_clustered_graphs(graphs, node.min.size = 45, node.size.multiplier = 0.5, node.max.size = 200,
#                      labels = F, to.pdf = F)
# 
# vis_clustered_graphs(graphs, node.min.size = 45, node.max.size = 200,
#                      labels = F, to.pdf = T, legend.node.size = 70, legend.label.size = 1)
# vis_clustered_graphs(graphs, node.min.size = 45, node.max.size = 200,
#                      labels = T, to.pdf = T, legend.node.size = 70, legend.label.size = 1)
# 
# vis_clustered_graphs(graphs, node.min.size = 45, node.max.size = 200,
#                      labels = F, to.pdf = T, legend.label.size = 3)
# vis_clustered_graphs(graphs, node.min.size = 45, node.max.size = 200,
#                      labels = F, to.pdf = T, legend.label.size = 3.5)
#library(igraph)
#E(graphs[[4]])$grp.density



# Choose layouts for groups of two and three
