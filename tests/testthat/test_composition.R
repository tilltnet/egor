library(testthat)
library(egor)

data(egor32)

netsize <- sapply(egor32$.alts, FUN = NROW)

composition(egor32, "alter.age", netsize)
composition(egor32, "alter.sex", netsize)
composition(egor32, "alter.sex", netsize, v_ego = egor32$sex)

alters.df <- as_alts_df(egor32)
composition(object = alters.df, v_alt = "alter.sex", netsize, v_ego = egor32$sex)


# Same names in egos and alter vars



egor32$.alts <- lapply(egor32$.alts, FUN = function(x) {
  names(x)[2:3] <- c("sex", "age")
  x
})

composition(egor32, "sex", netsize)
composition(egor32, "sex", netsize, v_ego = egor32$sex)

