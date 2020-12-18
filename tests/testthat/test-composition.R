context("test_composition.R")

test_that(
  "Composition commands work.",
  {
    e <- make_egor(3, 3)
    
    expect_error(composition(e, "age", T), NA, label = "composition absolute")
    expect_error(composition(e, "age"), NA, label = "composition proportional")

    expect_error(comp_ei(object = e, alt.attr = "sex", ego.attr = "sex"), NA)
    
    expect_error(comp_ply(e, "age", .f = egor:::fun_alts_diversity), 
                 NA, label = "comp_ply fun_alts_diversity")
    expect_error(comp_ply(e, "age", .f = egor:::fun_entropy, base = 7),
                 NA, label = "comp_ply fun_entropy")
    expect_error(comp_ply(e, "age", .f = egor:::fun_comp_ei, ego.attr = "age"),
                 NA, label = "comp_ply fun_comp_ei")
    # Example
    df <- make_egor(10, 32)
    expect_error(comp_ply(df, "age.years", sd, na.rm = TRUE), 
                 NA, "comp_ply Example")
    expect_error(alts_diversity_entropy(e, "age"), NA, label = "diversity_entropy")
    expect_error(alts_diversity_count(e, "age"), NA, label = "diversiy_count")
  }
)

test_that("composition() returns tbl_svy object, when ego_design present", {
  x <- make_egor(5, 32)
  
  x$ego$sampling_weight <-
    sample(1:10 / 10, 5, replace = TRUE)
  ego_design(x) <- list(weight = "sampling_weight")
  
  options(egor.results_with_design = TRUE)
  res <- composition(object = x, "age")
  expect_is(res, "tbl_svy")
})

test_that("proportional results sum up to 1", {
  e <- make_egor(3, 3)
  res <- composition(e, "age")
  expect_equal(rowSums(res[-1], na.rm = TRUE), c(1,1,1))
})

test_that("comp_ply works with missing alters",
          {
            e <- make_egor(3, 3)
            res <- e %>% 
              activate(alter) %>% 
              filter(.egoID != 3) %>% 
              comp_ei(alt.attr = "sex", ego.attr = "sex")
            expect_equal(res$.egoID, c(1, 2, 3))
            expect_equal(nrow(res), 3)
          })

test_that("comp_ply returns results as `tbl_svy` object",
          {
            x <- make_egor(5, 32)
            
            x$ego$sampling_weight <-
              sample(1:10 / 10, 5, replace = TRUE)
            ego_design(x) <- list(weight = "sampling_weight")
            
            options(egor.results_with_design = TRUE)
            res <- comp_ply(object = x,
                            alt.attr = "age.years",
                            .f = sd,
                            result.name = "age_sd")
            expect_is(res, "tbl_svy")
          })


test_that(".egoID class is conserved", {
  e <- make_egor(3, 3)
  expect_equal(
    class(comp_ei(e, alt.attr = "sex", ego.attr = "sex")$.egoID),
    class(e$ego$.egoID)
  )
  e2 <- 
    e %>% 
    mutate(.egoID = as.character(.egoID)) %>% 
    activate(alter) %>% 
    mutate(.egoID = as.character(.egoID)) %>% 
    activate(aatie) %>% 
    mutate(.egoID = as.character(.egoID))
  
  expect_equal(
    class(comp_ei(e2, alt.attr = "sex", ego.attr = "sex")$.egoID),
    class(e2$ego$.egoID)
  )
})


