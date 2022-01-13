test_that("left_join works with srvyr ego object", {
  x <- make_egor(5, 32)
  
  x$ego$sampling_weight <- sample(1:10/10, 5, replace = TRUE)
  ego_design(x) <- list(weight = "sampling_weight")
  
  res <- composition(x, age)
  
  expect_error(left_join(x, res, copy = TRUE), NA)
})


test_that("summary.egor works with ego_design", {
  x <- make_egor(5, 32)
  x$ego$sampling_weight <- sample(1:10/10, 5, replace = TRUE)
  ego_design(x) <- list(weight = "sampling_weight")
  
  expect_error(summary(object = x), NA)
  expect_warning(summary(object = x), NA)
})

test_that("ego_density works with ego_design", {
  x <- make_egor(5, 32)

  x$ego$sampling_weight <- sample(1:10/10, 5, replace = TRUE)
  ego_design(x) <- list(weight = "sampling_weight")
  library(srvyr)
  expect_error(ego_density(x), NA)
})

test_that("survey_mean and svymean work with ego_design", {
  x <- make_egor(5, 32)
  
  x$ego$sampling_weight <- sample(1:10/10, 5, replace = TRUE)
  ego_design(x) <- list(weight = "sampling_weight")
  
  options(egor.results_with_design = TRUE)

  expect_error(ego_density(x) %>% 
    srvyr::summarise(mean_dens = srvyr::survey_mean(density)), NA)
  
  expect_error(survey::svymean(~density, ego_density(x)), NA)

  
})
