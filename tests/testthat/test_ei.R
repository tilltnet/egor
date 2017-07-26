cat(" \nTestfile test_ei.r \n")

library(egor)

eigor <- generate.sample.ego.data(32, 20)


EI(eigor, var_name = "alter.sex")

aaties <- eigor$.aaties
alters <- eigor$.alts

EI(alters, aaties, var_name = "alter.sex")

data("alters32")
data("edges32")
EI(alters32, edges32, var_name = "alter.sex", altID = "alterID")

data("egor32")
EI(egor32, var_name = "alter.sex")


# Tests with non-factor variables

egor32$.alts <- lapply(egor32$.alts, FUN = function(x) {
  x$int_var <- sample(1:3, NROW(x), replace  = T)
  x
})

EI(egor32, var_name = "int_var")
