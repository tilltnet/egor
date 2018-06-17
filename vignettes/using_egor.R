## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  dev = "CairoPNG"
)

library(knitr)
library(egor)

## ------------------------------------------------------------------------
data("alters32")
data("egos32")
data("edges32") 

## ----echo=FALSE----------------------------------------------------------
alters32 %>%
  head() %>%
  kable(caption = "First rows of alter data.")

egos32 %>%
  head() %>%
  kable(caption = "First rows of ego data.")

edges32 %>%
  head() %>%
  kable(caption = "First rows of alter-alter tie data.")

## ------------------------------------------------------------------------
e1 <- egor(alters.df = alters32,
           egos.df = egos32,
           aaties = edges32,
           ID.vars = list(
             ego = "egoID",
             alter = "alterID",
             source = "Source",
             target = "Target"))
e1

## ----include=FALSE-------------------------------------------------------
library(dplyr)
library(purrr)

## ------------------------------------------------------------------------
summary(e1)

## ------------------------------------------------------------------------
ego_density(e1)

## ------------------------------------------------------------------------
composition(e1, "alter.age") %>%
  head() %>%
  kable()

## ------------------------------------------------------------------------
alts_diversity_count(e1, "alter.age")
alts_diversity_entropy(e1, "alter.age")

## ------------------------------------------------------------------------
homophily_ei(e1, "alter.age", "age")

## ------------------------------------------------------------------------
EI(e1, "alter.age") %>%
  head() %>%
  kable()

## ------------------------------------------------------------------------
e2 <- generate.sample.ego.data(15, 32)
comp_ply(e2, "age.years", sd, na.rm = TRUE)

## ------------------------------------------------------------------------
data("egor32")

# Simplify networks to clustered graphs, stored as igraph objects
graphs <- clustered_graphs(egor32, "age") 

# Visualise
par(mar=c(0,0,0,0))
vis_clustered_graphs(graphs[1:3], 
                     node.size.multiplier = 10, 
                     edge.width.multiplier = 5,
                     label.size = 0.6)

graphs2 <- clustered_graphs(generate.sample.ego.data(400, 200)[1:4], "country") 

vis_clustered_graphs(graphs2[1:4], 
                     node.size.multiplier = 2, 
                     edge.width.multiplier = 15,
                     label.size = 0.6,
                     labels = TRUE)

## ------------------------------------------------------------------------
par(mar=c(0,0,0,0))
purrr::walk(as_igraph(egor32)[1:4], plot)
purrr::walk(as_network(egor32)[1:4], plot)

## ------------------------------------------------------------------------
as_alts_df(egor32, include.ego.vars = TRUE) %>%
  head() %>%
  kable(caption = "First rows of global alters data frame.")

## ------------------------------------------------------------------------
as_ties_df(egor32, include.alt.vars = TRUE) %>%
  head() %>%
  kable(caption = "First rows of global alter-alter tie data frame.")

