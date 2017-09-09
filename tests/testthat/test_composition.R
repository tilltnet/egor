library(testthat)
library(egor)

data(egor32)

netsize <- sapply(egor32$.alts, FUN = NROW)

composition(egor32, "alter.age", netsize)
composition(egor32, "alter.sex", netsize)
composition(egor32, "alter.sex", netsize, v_ego = egor32$sex)

alters.df <- as_alts_df(egor32)
composition(alters.df, "alter.sex", netsize, v_ego = egor32$sex)
