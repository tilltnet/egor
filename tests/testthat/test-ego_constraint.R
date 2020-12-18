test_that("ego_constraint works without weight", {
  e1 <- make_egor(32, 22)
  
  expect_error(ego_constraint(object = e1), NA)
})

test_that("ego_constraint works with weight", {
  e1 <- make_egor(32, 22)
  e1 <-
    e1 %>%
    activate(alter) %>%
    mutate(weight = sample(1:3 / 3, nrow(.$alter), replace = T))
  
  expect_error(ego_constraint(object = e1, weight = "weight"), NA)
})

test_that("ego_constraint() returns tbl_svy object, when ego_design present", {
  x <- make_egor(5, 32)
  
  x$ego$sampling_weight <-
    sample(1:10 / 10, 5, replace = TRUE)
  ego_design(x) <- list(weight = "sampling_weight")
  options(egor.results_with_design = TRUE)
  res <- ego_constraint(object = x)
  
  expect_is(res, "tbl_svy")
})
