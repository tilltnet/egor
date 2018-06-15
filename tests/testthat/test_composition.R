context("test_composition.R")

library(testthat)
library(egor)

data(egor32)

composition(egor32, "age", T)
composition(egor32, "age")

egor_diversity(egor32, "age")
homophily_ei(egor32, "sex")


comp_ply(egor32, "age", .f = egor:::fun_alts_diversity)
comp_ply(egor32, "age", .f = egor:::fun_entropy, base = 7)
comp_ply(egor32, "age", .f = egor:::fun_homophily_ei, ego.attr = "age")


alts_diversity_entropy(egor32, "age")
alts_diversity_count(egor32, "age")
