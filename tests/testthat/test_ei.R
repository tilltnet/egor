context("test_ei.R")

library(egor)

test_that(
  "EI() works.",
  {
    eigor <- make_egor(6, 20)
    
    expect_error(EI(object = eigor, alt.attr = "age"), NA, label = "regular")
    expect_error(EI(eigor, alt.attr = "sex"), NA, label = "regular 2")
    expect_error(EI(eigor, sex), NA, label = "NSE input.")
    
    
    
    eigor$.alts <- lapply(eigor$.alts, FUN = function(x) {
      x$int_var <- sample(1:3, NROW(x), replace  = T)
      x
    })
    expect_error(EI(eigor, alt.attr = "int_var"), NA, "non-factor variable")
    
    # Issue #16 by mbojan
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
              ID.vars = list(ego ="id_ego", alter="id_alter", source="from", target="to"))
    expect_error(
      EI(object = e, alt.attr = "female"),
      NA, label = "Missing aaties.")
    
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
    expect_error(
      EI(object = e, alt.attr = "female"),
      NA, label = "Missing aaties, only one group in alts.")
  }
)