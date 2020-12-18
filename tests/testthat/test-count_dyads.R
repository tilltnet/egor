test_that("count_dyads works", {
  data("egor32")
  
  expect_error(count_dyads(object = egor32, alter_var_name = "country"), NA)
  
  expect_error(count_dyads(
    object = egor32,
    alter_var_name = "country",
    prefix = "pre"
  ),
  NA)
  
  expect_error(count_dyads(
    object = egor32,
    alter_var_name = "country",
    return_as = "long"
  ),
  NA)
  
  # Errors for wrong arguments
  expect_error(count_dyads(
    object = egor32,
    alter_var_name = "country3",
    return_as = "asd"
  ))
  
  expect_error(count_dyads(
    object = egor32,
    alter_var_name = "country",
    return_as = "asd"
  ))
})

test_that("count_dyads() returns tbl_svy object, when ego_design present", {
  x <- make_egor(5, 32)
  
  x$ego$sampling_weight <-
    sample(1:10 / 10, 5, replace = TRUE)
  ego_design(x) <- list(weight = "sampling_weight")
  
  options(egor.results_with_design = TRUE)
  res <- count_dyads(object = x, "sex")
  expect_is(res, "tbl_svy")
})
