cat(" \nTestfile test_ei.r \n")

library(egor)

eigor <- generate.sample.ego.data(32, 20)


#View(EI(eigor, var_name = "alter.sex"))

aaties <- eigor$.aaties
alters <- eigor$.alts

EI(object = alters, aaties =  aaties, var_name = "sex")

data("alters32")
data("edges32")
EI(alters32, edges32, var_name = "alter.sex", altID = "alterID")

data("egor32")

EI(egor32, var_name = "sex")


# Tests with non-factor variables

egor32$.alts <- lapply(egor32$.alts, FUN = function(x) {
  x$int_var <- sample(1:3, NROW(x), replace  = T)
  x
})

EI(egor32, var_name = "int_var")


# Issue #16 by mbojan
library(egor)

egodf <- data.frame(
  id_ego = 1:2,
  female= 0:1
)

alterdf <- data.frame(
  id_ego = c(1,1),
  id_alter = 1:2,
  female = c(FALSE, TRUE)
)

aaties <- data.frame(
  id_ego = 1,
  from = 1,
  to = 2,
  close = 100
)


e <- egor(alters.df = alterdf, egos.df = egodf, aaties.df = aaties, 
          ID.vars = list(ego="id_ego", alter="id_alter", source="from", target="to"))

EI(object = e, var_name = "female",  egoID="id_ego", altID="id_alter")

# same test with more complete aaties data but only one group in the network

alterdf <- data.frame(
  id_ego = rep(1,3),
  id_alter = 1:3,
  female = rep(TRUE, 3)
)

aaties <- data.frame(
  id_ego = rep(1,3),
  from = c(1,1,2),
  to = c(2,3,3),
  close = rep(100, 3)
)

e <- egor(alterdf, egodf, aaties, 
          ID.vars = list(ego="id_ego", alter="id_alter", source="from", target="to"))

EI(object = e, var_name = "female",  egoID="id_ego", altID="id_alter")
