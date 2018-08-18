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
      NA, label = "Missing aaties.")
  }
)

test_that("comp_ei handles extreme values (only one group in alts) correctly",
          {
            data("egor32")
            
            make_nas <- function(var) {
              if(is.tibble(var)) var <- var[[1]]
              var[sample(1:length(var), 1)] <- NA
              var
            }
            
            egor32$sexy <- make_nas(egor32$sex)
            egor32 <- egor32 %>% 
              mutate(.alts = purrr::map(.alts, function(x) 
                mutate(x, sex = make_nas(sex))))
            
            e <-  egor32[1:4, ]
            
            e$.alts[[1]]$sex <- factor("w")
            e$.alts[[2]]$sex <- factor("m")
            e$.alts[[3]]$sex[1:length(e$.alts[[3]]$sex)/2] <- factor("w")
            e$.alts[[3]]$sex[21:40] <- factor("m")
            e$.alts[[4]]$sex[1:15] <- factor("m")
            e$.alts[[4]]$sex[16:20] <- factor("w")
            
            e[5:60, ] <- e[4, ]
            
            for(i in 5:25) {
              j <- i - 5
              e$.alts[[i]]$sex[1:j] <- factor("m")
              e$.alts[[i]]$sex[(j):20] <- factor("w")
            }
            
            expect_false(all(is.na(comp_ei(e, "sex", "sex"))))
          })

test_that("comp_ei handles character vectors correctly",
          {
            data("egor32")
            e1 <- egor32
            e1$sex <- as.character(e1$sex)
            e1$.alts[[1]]$sex <- as.character(e1$.alts[[1]]$sex)
            
            
            
            expect_equal(comp_ei(e1, "sex", ego.attr = "sex"),
                  comp_ei(egor32, "sex",ego.attr = "sex"))
            
          })

