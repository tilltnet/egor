context("test_conversions.R")

library(testthat)
library(dplyr)
library(egor)

data("egor32")

# as_network()

egor32$.alts <- lapply(egor32$.alts, FUN = function(x) {
  x$int_var <- sample(1:3, NROW(x), replace  = T)
  x
})

egor32$networks <- network::as.network(egor32)
network::list.vertex.attributes(egor32$networks[[1]])
network::list.edge.attributes(egor32$networks[[1]])
network::get.edge.attribute(egor32$networks[[1]], "na")
network::get.edge.attribute(egor32$networks[[1]], "weight")

egor32$networks <- as_network(egor32, include.ego = T, ego.attrs = "sex", ego.alter.weights = "int_var")
egor32$networks <- as_network(egor32, include.ego = T)
network::get.edge.attribute(egor32$networks[[1]], "weight")
network::get.vertex.attribute(egor32$networks[[1]], "sex")



network::get.vertex.attribute(egor32$networks[[1]], "vertex.names")
network::list.vertex.attributes(egor32$networks[[1]])



egor32$igraphs <- as_igraph(egor32, include.ego = T)
igraph::get.vertex.attribute(egor32$igraphs[[1]])
egor32$igraphs <- as_igraph(egor32, include.ego = T, ego.attrs = c("sex", "age"), ego.alter.weights = "int_var")
igraph::get.vertex.attribute(egor32$igraphs[[1]])
igraph::get.edge.attribute(egor32$igraphs[[1]])


names(egor32)
egor32$.alts <- lapply(egor32$.alts, FUN = function(x) {
  names(x)[2:4] <- c("sex", "age", "weight")
  x
})

egor32$igraphs <- as_igraph(egor32, include.ego = T, ego.attrs = c("sex", "age"), ego.alter.weights = "weight")
egor32$networks <- as_network(egor32, include.ego = T, ego.attrs = c("sex", "age"), ego.alter.weights = "weight")

igraph::V(egor32$igraphs[[1]])$age

# Global alters dataframe
as_alts_df(egor32, "ID")
as_alts_df(egor32)


#egor32 %>% select(-egoID) %>% as_alts_df(F)
# 
# Global alter-alter ties
 as_aaties_df(egor32)

as_aaties_df(object = egor32, include.alt.vars = T)

# Same names in egos and alter vars
 egor32$.alts <- lapply(egor32$.alts, FUN = function(x) {
   names(x)[2:3] <- c("sex", "age")
   x
 })
# 
# as_alts_df(egor32, include.ego.vars = T) #!# add helpful refixes. Only if duplicated names present or always?

