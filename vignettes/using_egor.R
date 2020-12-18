## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  dev.args = list(type = if(capabilities('cairo')) "cairo" else "png"))

library(knitr)
library(egor)

## -----------------------------------------------------------------------------
data("alters32")
data("egos32")
data("aaties32") 

## ----echo=FALSE---------------------------------------------------------------
alters32 %>%
  head() %>%
  kable(caption = "First rows of alter data.")

egos32 %>%
  head() %>%
  kable(caption = "First rows of ego data.")

aaties32 %>%
  head() %>%
  kable(caption = "First rows of alter-alter tie data.")

## -----------------------------------------------------------------------------
e1 <- egor(alters = alters32,
           egos = egos32,
           aaties = aaties32,
           ID.vars = list(
             ego = ".EGOID",
             alter = ".ALTID",
             source = ".SRCID",
             target = ".TGTID"))
e1

## -----------------------------------------------------------------------------
e1[e1$ego$age.years > 35, ]

## -----------------------------------------------------------------------------
subset(e1, e1$alter$sex == "w", unit = "alter")

## -----------------------------------------------------------------------------
subset(e1, e1$aatie$weight > 0.5, unit = "aatie")

## -----------------------------------------------------------------------------
e1 %>% 
  filter(income > 36000)

e1 %>% 
  activate(alter) %>% 
  filter(country %in% c("USA", "Poland"))

e1 %>% 
  activate(aatie) %>% 
  filter(weight > 0.7)

## -----------------------------------------------------------------------------
summary(e1)

## -----------------------------------------------------------------------------
ego_density(e1)

## -----------------------------------------------------------------------------
composition(e1, "age") %>%
  head() %>%
  kable()

## -----------------------------------------------------------------------------
alts_diversity_count(e1, "age")
alts_diversity_entropy(e1, "age")

## -----------------------------------------------------------------------------
comp_ei(e1, "age", "age")

## -----------------------------------------------------------------------------
EI(e1, "age") %>%
  head() %>%
  kable()

## -----------------------------------------------------------------------------
# return results as "wide" tibble
  count_dyads(
    object = e1,
    alter_var_name = "country"
  )

# return results as "long" tibble
  count_dyads(
    object = e1,
    alter_var_name = "country",
    return_as = "long"
  )

## -----------------------------------------------------------------------------
e2 <- make_egor(15, 32)
comp_ply(e2, "age.years", sd, na.rm = TRUE)

## -----------------------------------------------------------------------------
data("egor32")

# Simplify networks to clustered graphs, stored as igraph objects
graphs <- clustered_graphs(egor32, "age") 

# Visualize
par(mfrow = c(2,2), mar = c(0,0,0,0))
vis_clustered_graphs(graphs[1:3], 
                     node.size.multiplier = 1, 
                     edge.width.multiplier = 1,
                     label.size = 0.6)

graphs2 <- clustered_graphs(make_egor(50, 50)[1:4], "country") 

vis_clustered_graphs(graphs2[1:3], 
                     node.size.multiplier = 1, 
                     edge.width.multiplier = 3,
                     label.size = 0.6,
                     labels = FALSE)

## -----------------------------------------------------------------------------
par(mar = c(0, 0, 0, 0), mfrow = c(2, 2))
purrr::walk(as_igraph(egor32)[1:4], plot)
purrr::walk(as_network(egor32)[1:4], plot)

## ----fig.height=6, fig.width=8------------------------------------------------
plot(egor32)

## ----fig.height=6, fig.width=8------------------------------------------------
plot(make_egor(32,16), venn_var = "sex", pie_var = "country", type = "egogram")

