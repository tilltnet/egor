context("test_composition.R")

library(testthat)
library(egor)

data(egor32)

composition(egor32, "age", T)
composition(egor32, "age")

comp_ei(egor32, "sex", "sex")


comp_ply(egor32, "age", .f = egor:::fun_alts_diversity)
comp_ply(egor32, "age", .f = egor:::fun_entropy, base = 7)
comp_ply(egor32, "age", .f = egor:::fun_comp_ei, ego.attr = "age")


alts_diversity_entropy(egor32, "age")
alts_diversity_count(egor32, "age")

# Example
df <- make_egor(10, 32)
comp_ply(df, "age.years", sd, na.rm = TRUE)
