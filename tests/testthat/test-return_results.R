test_that("return_results() works when $ego is `srvyr` object", {
  x <- make_egor(5, 32)
  
  x$ego$sampling_weight <- sample(1:10/10, 5, replace = TRUE)
  ego_design(x) <- list(weight = "sampling_weight")
  
  res <- composition(x, age)
  res <- res$variables
  
  expect_error(egor:::return_results(x, res), NA)
})

test_that("return_results() works when $ego is NOT a `srvyr` object", {
  x <- make_egor(5, 32)
  
  res <- composition(x, age)
  
  expect_error(egor:::return_results(x, res), NA)
})
